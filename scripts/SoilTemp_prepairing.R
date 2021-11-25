
library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(cowplot)
library(sf)
library(readxl)

# Tomst

d <- fread("C:/datacloud/biogeoclimate/microclimate/data/logger/all_data.csv") %>% 
  filter(!area %in% c("SAA","RAS","RAR")) %>% 
  select(id_code, datetime, error_tomst, T1:T3, moist)

d %>% mutate(T1 = ifelse(error_tomst %in% c(1,2,4), NA, T1),
             T2 = ifelse(error_tomst %in% c(1,2), NA, T2),
             T3 = ifelse(error_tomst > 0, NA, T3),
             moist = ifelse(error_tomst %in% c(1,2), NA, moist),
             moist = ifelse(T1 < 1, NA, moist)) -> d

d %>% filter(if_any(T1:moist, ~ !is.na(.))) %>% 
  select(-error_tomst) -> d

d %>% mutate(Year = year(datetime),
             Month = month(datetime),
             Day = mday(datetime),
             Time = format(datetime, "%H:%M", tz = "GMT")) -> d

d %>% mutate(Raw_data_identifier = paste0(id_code,"_tomst")) %>% 
  rename(Soil_moisture = moist) %>% 
  select(Raw_data_identifier, Year, Month, Day, Time, T1, T2, T3, Soil_moisture) -> d

############################################
# META

p <- st_read("data/geodata/points_all.gpkg")

# Function for site identifiers
add_zeros <- function(x){
  if(nchar(x) == 1){
    return(as.character(paste0("00",x)))
  }
  if(nchar(x) == 2){
    return(as.character(paste0("0",x)))
  }
  if(nchar(x) > 2){
    return(as.character(x))
  }
}

p %>% mutate(Location_code = paste0(area, unlist(lapply(id, add_zeros))),
             Latitude = st_coordinates(.)[,"Y"],
             Longitude = st_coordinates(.)[,"X"]) %>% 
  filter(logger == "Tomst") %>% 
  st_drop_geometry() %>% 
  select(Location_code:Longitude) %>% 
  mutate(Raw_data_identifier = paste0(Location_code, "_tomst"),
         EPSG = 4326,
         GPS_accuracy = 5) -> p


d %>% select(Raw_data_identifier, T1:Soil_moisture) %>% 
  mutate(Country_code = "FI") %>% 
  pivot_longer(cols = T1:Soil_moisture, names_to = "Sensor_code") %>% 
  group_by(Country_code, Raw_data_identifier, Sensor_code) %>% 
  count() %>% 
  select(-n) -> meta

meta %>% mutate(Location_code = gsub("_tomst", "", Raw_data_identifier),
                Logger_code = Raw_data_identifier) %>% 
  relocate(Location_code, .after = Country_code) %>% 
  relocate(Logger_code, .after = Location_code) %>% 
  mutate(Sensor_height = ifelse(Sensor_code == "Soil_moisture", -15, NA),
         Sensor_height = ifelse(Sensor_code == "T1", -6, Sensor_height),
         Sensor_height = ifelse(Sensor_code == "T2", 2, Sensor_height),
         Sensor_height = ifelse(Sensor_code == "T3", 15, Sensor_height)) %>% 
  relocate(Raw_data_identifier, .after = Sensor_height) %>% 
  mutate(Microclimate_measurement = "Temperature",
         Microclimate_measurement = ifelse(Sensor_code == "Soil_moisture", "Soil_moisture", Microclimate_measurement)) -> meta


left_join(meta, p) %>% 
  mutate(Logger_brand = "Tomst",
         Logger_type = "TMS-4",
         Sensor_accuracy = 0.5,
         Logger_shielding = "Shield") %>% 
  mutate(Logger_shielding = ifelse(Sensor_code %in% c("T1","Soil_moisture"), "No shield", Logger_shielding)) -> meta



d %>% mutate(date = as_date(paste(Year,Month,Day,sep = "_"))) %>% 
  select(Raw_data_identifier, date, T1:Soil_moisture) %>% 
  pivot_longer(cols = T1:Soil_moisture, names_to = "Sensor_code") %>% 
  filter(!is.na(value)) %>% 
  group_by(Raw_data_identifier, Sensor_code) %>% 
  summarise(mind = min(date),
            maxd = max(date)) %>% 
  mutate(Start_date_year = year(mind),
         Start_date_month = month(mind),
         Start_date_date = day(mind),
         End_date_year = year(maxd),
         End_date_month = month(maxd),
         End_date_date = day(maxd)) %>% 
  select(-mind,-maxd) -> dates

left_join(meta, dates) %>% 
  mutate(Temporal_resolution = 15,
         Timezone = "UTC") %>% 
  mutate(Species_composition = ifelse(grepl("AIL",Location_code)|grepl("MAL",Location_code)|grepl("VAR",Location_code), "yes", "no")) -> meta

trait_ids <- unique(fread("C:/Users/OMISTAJA/Documents/repos/Fennoscandia_plant_traits/trait_data/Kilpis_combined_leaf_traits.csv")$site)

meta %>% mutate(Species_trait = "no") %>% 
  mutate(Species_trait = ifelse(grepl("VAR",Location_code), "yes", Species_trait),
         Species_trait = ifelse(Location_code %in% trait_ids, "yes", Species_trait)) -> meta

meta %>% mutate(Plot_size = NA,
                Forest_canopy_cover = NA,
                Data_open_access = "yes",
                Meta_data_open_access = "yes",
                FirstName = "Pekka",
                LastName = "Niittynen",
                Email = "poniitty@gmail.com",
                Institute = "University of Helsinki",
                Other_contributors = "Miska Luoto; Juha Aalto; Julia Kemppinen; Tuuli Rissanen; Vilna Tyystjärvi") -> meta

# HABITAT CLASSES

# Tiilikka
boreal_forests <- paste0("TII",unlist(lapply(c(50,53,29,16,51,7,35,21,3,34,4,47,
                                               37,18,48,46,1,31,44,54,19,12),add_zeros)))

meta %>% 
  mutate(Habitat_type = NA,
         Habitat_sub_type = NA) %>% 
  mutate(Habitat_type = ifelse(grepl("TII",Location_code), "5",Habitat_type),
         Habitat_type = ifelse(Location_code %in% boreal_forests, "1",Habitat_type),
         Habitat_sub_type = ifelse(grepl("TII",Location_code), "5.4",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% boreal_forests, "1.1",Habitat_sub_type)) -> meta


# Pisa
wetlands <- paste0("PIS",unlist(lapply(c(4,10,19),add_zeros)))

meta %>% 
  mutate(Habitat_type = ifelse(grepl("PIS",Location_code), "1",Habitat_type),
         Habitat_type = ifelse(Location_code %in% wetlands, "5",Habitat_type),
         Habitat_sub_type = ifelse(grepl("PIS",Location_code), "1.1",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% wetlands, "5.4",Habitat_sub_type)) -> meta


# Hyytiälä
wetlands <- paste0("HYY",unlist(lapply(c(39,40,50,53,30,3,45,29,19,37,4,52,
                                         36,7,28,10,55,38,27,9,24,21,13,49),add_zeros)))

meta %>% 
  mutate(Habitat_type = ifelse(grepl("HYY",Location_code), "1",Habitat_type),
         Habitat_type = ifelse(Location_code %in% wetlands, "5",Habitat_type),
         Habitat_sub_type = ifelse(grepl("HYY",Location_code), "1.1",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% wetlands, "5.4",Habitat_sub_type)) -> meta


# Karkali

tempe_forests <- paste0("KAR",unlist(lapply(c(44,8,12),add_zeros)))
wetlands <- paste0("KAR",unlist(lapply(c(47,55),add_zeros)))

meta %>% 
  mutate(Habitat_type = ifelse(grepl("KAR",Location_code), "1",Habitat_type),
         Habitat_type = ifelse(Location_code %in% wetlands, "5",Habitat_type),
         Habitat_sub_type = ifelse(grepl("KAR",Location_code), "1.1",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% tempe_forests, "1.4",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% wetlands, "5.4",Habitat_sub_type)) -> meta

# Värriö

suba_forests <- paste0("VAR",unlist(lapply(c(48,36,16,77,31,25,50,2,54),add_zeros)))
suba_grassland <- paste0("VAR",unlist(lapply(c(42,32,52),add_zeros)))
wetlands <- paste0("VAR",unlist(lapply(c(40,3,27,38,51,53,11),add_zeros)))

meta %>% 
  mutate(Habitat_type = ifelse(grepl("VAR",Location_code), "1",Habitat_type),
         Habitat_type = ifelse(Location_code %in% suba_grassland, "4",Habitat_type),
         Habitat_type = ifelse(Location_code %in% wetlands, "5",Habitat_type),
         Habitat_sub_type = ifelse(grepl("VAR",Location_code), "1.1",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% suba_forests, "1.2",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% suba_grassland, "4.2",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% wetlands, "5.4",Habitat_sub_type)) -> meta


# Malla & Ailakka

env <- fread("data/all_env_variables.csv") %>% 
  filter(area %in% c("AIL","MAL")) %>% 
  filter(logger == "Tomst")

wetlands <- env %>% filter(wetland > 0) %>% pull(id_code)
suba_forests <- env %>% filter(canopy_cover2m_30m > 25) %>% pull(id_code)

meta %>% 
  mutate(Habitat_type = ifelse(grepl("AIL",Location_code) | grepl("MAL",Location_code), "4",Habitat_type),
         Habitat_type = ifelse(Location_code %in% suba_forests, "1",Habitat_type),
         Habitat_type = ifelse(Location_code %in% wetlands, "5",Habitat_type),
         Habitat_sub_type = ifelse(grepl("AIL",Location_code) | grepl("MAL",Location_code), "4.2",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% suba_forests, "1.2",Habitat_sub_type),
         Habitat_sub_type = ifelse(Location_code %in% wetlands, "5.10",Habitat_sub_type)) -> meta

meta %>% relocate(Habitat_type:Habitat_sub_type, .after = Logger_shielding) -> meta

fwrite(meta, "data/SoilTemp/SoilTemp_FI_PN_FINTOMSTS_meta.csv")
fwrite(d, "data/SoilTemp/SoilTemp_FI_PN_FINTOMSTS.csv")

