
library(tidyverse)
library(lubridate)
#remotes::install_github("rOpenGov/fmi2")
library(fmi2)
library(sf)


station_data <- read_delim("data/extra/fmi_stations.csv", delim = ";", locale = locale(encoding = "UTF-8"))
station_xy <- st_as_sf(station_data, coords = c("Lon","Lat"), crs = 4326) %>% 
  st_transform(crs = 32635)

#############################################################################

p <- st_read("data/geodata/points_all.gpkg") %>% 
  st_transform(crs = 32635)

# Check which stations are within 50 km of our study sites
station_xy$within50km <- unlist(lapply(st_is_within_distance(station_xy, p, 50000), function(x) !is_empty(x)))

station_xy %>% filter(within50km) %>% 
  filter(grepl("weather", Groups, ignore.case = T)) -> selected_stations

plot(st_geometry(selected_stations))

####################################################################################
# DOWNLOAD DATA

# Daily

startdate <- "1990-01-01"
enddate <- Sys.Date()

days <- seq(as_date(startdate), as_date(enddate), by  = "month")

df <- data.frame()
for(i in 1:NROW(selected_stations)){
  # i <- 9
  print(selected_stations$Name[i])
  
  for(ii in days){
    
    print(as_date(ii))
    
    tryCatch({
      d <- fmi2::obs_weather_daily(starttime = as_date(ii),
                                   endtime = (as_date(ii) + months(1)) - 1,
                                   fmisid = selected_stations$FMISID[i])
      d %>% st_drop_geometry() %>% 
        #filter(complete.cases(.)) %>% 
        pivot_wider(names_from = "variable", values_from = "value") %>% 
        mutate(FMISID = selected_stations$FMISID[i]) -> d
      
      df <- bind_rows(df, d)
    }, error = function(e) { print("NO DATA FOR THESE DATES") })
    #Sys.sleep(1)
  }
}

write_csv(df, "output/fmi_daily.csv")

# HOURLY

startdate <- "2018-01-01"
enddate <- Sys.Date()

days <- seq(as_date(startdate), as_date(enddate), by  = "month")

df <- data.frame()
for(i in 1:NROW(selected_stations)){
  # i <- 1
  print(selected_stations$Name[i])
  
  for(ii in days){
    
    print(as_date(ii))
    
    tryCatch({
      d <- fmi2::obs_weather_hourly(starttime = as_date(ii),
                                    endtime = (as_date(ii) + months(1)) - 1,
                                    fmisid = selected_stations$FMISID[i])
      d %>% st_drop_geometry() %>% 
        filter(complete.cases(.)) %>% 
        pivot_wider(names_from = "variable", values_from = "value") %>% 
        mutate(area = selected_stations$area[i],
               FMISID = selected_stations$FMISID[i]) -> d
      
      df <- bind_rows(df, d)
    }, error = function(e) { print("NO DATA FOR THESE DATES") })
  }
}

write_csv(df, "output/fmi_hourly.csv")



