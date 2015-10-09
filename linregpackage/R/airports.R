#' A function that creates a plot showing mean delay of flights.
#' 

library(ggplot2)
library(dplyr)
library(nycflights13)

visualize_airport_delays <- function () {
  
  data(flights)
  #creates a new data.frame with only the columns we need included
  flights_origin_delays <- dplyr::select(flights, origin, dep_delay)  
  fod <- flights_origin_delays
  
  flights_arrival_delays <- dplyr::select(flights, dest, arr_delay)  
  fad <- flights_arrival_delays
  
  #group by origin (EWR, LGA or JFK) 
  by_origin <- group_by(fod, origin)
  
  #group by destination
  by_dest <- group_by(fad, dest)
  
  #creates data.frame that includes the origins(=departure airports) and mean of the departure delays for each origin
  dep_delay <- summarise(by_origin,
                     delay = mean(dep_delay, na.rm = TRUE))
  
  #creates data.frame that includes the destination(=arrival airports) and mean of the arrival delays for each destination
  arr_delay <- summarise(by_dest,
                     delay = round(mean(arr_delay, na.rm = TRUE)))
 
  data(airports)
  #creates a new data.frame with only the columns we need included, faa=FAA airport code, lat=latitude, lon=longitude 
  airport_origin_lat_lon <- dplyr::select(airports, origin=faa,lat,lon)
  aoll <- airport_origin_lat_lon
  
  airport_destination_lat_lon <- dplyr::select(airports, dest=faa,lat,lon)
  adll <- airport_destination_lat_lon
  
  #combines dep_delay and by_origin into a new data.frame
  dep_delay_df <- dplyr::left_join(dep_delay, aoll, by="origin")
  
  #combines arr_delay and by_dest into a new data.frame
  arr_delay_df <- dplyr::left_join(arr_delay, adll, by="dest")
  
  #plot departure delays
  p <- ggplot(dep_delay_df, aes(x = lat, y = lon)) + 
    geom_text(aes(label = delay), hjust = 0.2, size = 3) +
    labs(x = "latitude", y = "longitude") +
    ggtitle("The mean departure delay of flights for different airports")
  print(p)
  
  #plot arrival delays
  p <- ggplot(arr_delay_df, aes(x = lat, y = lon)) + 
    geom_text(aes(label = delay), hjust = 0.2, size = 3) +
    labs(x = "latitude", y = "longitude") +
    ggtitle("The mean arrival delay of flights for different airports")
  print(p)
  
}
