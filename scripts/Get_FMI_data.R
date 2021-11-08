
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

# Empty dataframe or previous data
if(file.exists("output/fmi_daily.csv")){
  df <- as_tibble(read.csv("output/fmi_daily.csv")) %>% 
    mutate(time = as_date(time))
} else {
  df <- data.frame()
}

if(file.exists("output/fmi_daily.csv")){
  startdate <- max(df$time)
} else {
  startdate <- "1990-03-01"
}
enddate <- Sys.Date()

days <- seq(as_date(floor_date(as_date(startdate)-months(2))), as_date(enddate), by  = "month")


for(i in 1:NROW(selected_stations)){
  # i <- 2
  print(selected_stations$Name[i])
  
  for(ii in days){
    # ii <- "2021-08-21"
    print(as_date(ii))
    
    tryCatch({
      d <- fmi2::obs_weather_daily(starttime = as_date(ii),
                                   endtime = (as_date(ii) + months(1)) - 1,
                                   fmisid = selected_stations$FMISID[i])
      d %>% st_drop_geometry() %>% 
        filter(complete.cases(.)) %>% 
        pivot_wider(names_from = "variable", values_from = "value") %>% 
        mutate(FMISID = selected_stations$FMISID[i]) -> d
      
      df <- bind_rows(df, d)
    }, error = function(e) { print("NO DATA FOR THESE DATES") })
    #Sys.sleep(1)
  }
}

df %>% filter(!duplicated(df %>% select(time, FMISID), fromLast = T)) -> df

write_csv(df, "output/fmi_daily.csv")

# HOURLY

# Empty dataframe or previous data
if(file.exists("output/fmi_hourly.csv")){
  df <- as_tibble(read.csv("output/fmi_hourly.csv"))
} else {
  df <- data.frame()
}

if(file.exists("output/fmi_hourly.csv")){
  startdate <- max(df$time)
} else {
  startdate <- "2018-03-01"
}
enddate <- Sys.Date()

days <- seq(as_date(floor_date(as_date(startdate)-months(2))), as_date(enddate), by  = "month")

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
        mutate(FMISID = selected_stations$FMISID[i]) -> d
      
      df <- bind_rows(df, d)
    }, error = function(e) { print("NO DATA FOR THESE DATES") })
  }
}

df %>% filter(!duplicated(df %>% select(time, FMISID), fromLast = T)) -> df

write_csv(df, "output/fmi_hourly.csv")

# Station summary

selected_stations

df <- as_tibble(read.csv("output/fmi_daily.csv")) %>% 
  mutate(time = as_date(time))

df %>% group_by(FMISID) %>% 
  summarise(first_date = min(time),
            last_date = max(time),
            tdays = sum(!is.na(tday)),
            rrdays = sum(!is.na(rrday))) -> aggr

selected_stations <- full_join(selected_stations, aggr)

bind_cols(p, p %>% st_coordinates() %>% as.data.frame()) %>% 
  st_drop_geometry() %>% 
  group_by(area) %>% 
  summarise(X = mean(X),
            Y = mean(Y)) %>% 
  st_as_sf(., coords = c("X","Y"), crs = st_crs(p)) -> centroids


selected_stations %>% 
  mutate(KAR = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "KAR"))/1000),1),
         HYY = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "HYY"))/1000),1),
         PIS = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "PIS"))/1000),1),
         TII = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "TII"))/1000),1),
         VAR = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "VAR"))/1000),1),
         AIL = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "AIL"))/1000),1),
         SAA = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "SAA"))/1000),1),
         MAL = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "MAL"))/1000),1),
         RAS = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "RAS"))/1000),1)) -> selected_stations


selected_stations$closest_area <- names(selected_stations %>% select(KAR:RAS) %>% st_drop_geometry())[apply(selected_stations %>% select(KAR:RAS) %>% st_drop_geometry(), 1, function(x) which.min(x))]

bind_cols(selected_stations, selected_stations %>% st_coordinates() %>% as.data.frame()) %>% 
  st_drop_geometry() %>% 
  select(-Groups,-within50km) %>% 
  relocate(X, Y, .after = Elevation) %>% 
  relocate(closest_area, .after = rrdays) %>% 
  rename(x_utm35 = X,
         y_utm35 = Y) -> selected_stations_df

write_csv(selected_stations_df, "output/fmi_stations.csv")
