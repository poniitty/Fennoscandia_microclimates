###############################################################################
# Gather and combine microclimate data from study area specific Github repositories
#

library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)

# Download datasets
# Rastigaisa
d1 <- read_csv("https://raw.githubusercontent.com/poniitty/Rastigaisa_microclimate/main/output/tomst_data_daily.csv")

# Kilpisjärvi
d2 <- read_csv("https://raw.githubusercontent.com/poniitty/kilpisjarvi_microclimate/main/output/tomst_data_daily.csv") %>% 
  mutate(site = gsub("RA","RAR",site))

# Värriö
d3 <- read_csv("https://raw.githubusercontent.com/poniitty/varrio_microclimate/main/output/tomst_data_daily.csv")

# Tiilikka
d4 <- read_csv("https://raw.githubusercontent.com/poniitty/Tiilikka_microclimates/main/output/tomst_data_daily.csv")

# Pisa
d5 <- read_csv("https://raw.githubusercontent.com/poniitty/Pisa_microclimates/main/output/tomst_data_daily.csv")

# Hyytiälä
d6 <- read_csv("https://raw.githubusercontent.com/poniitty/Hyytiala_microclimates/main/output/tomst_data_daily.csv")

# Karkali
d7 <- read_csv("https://raw.githubusercontent.com/poniitty/Karkali_microclimate/main/output/tomst_data_daily.csv")

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

# Combine all data and edit the sites ID's
bind_rows(d1 %>% mutate(site = paste0("RAS",unlist(lapply(site, add_zeros)))),
          d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> daily

# Check
unique(daily$area)

# Daily data ready!
# write_csv(daily, "data/tomst_data_daily.csv")

############################################################################
# haxo & hobo data

# Kilpisjärvi
d2 <- read_csv("https://raw.githubusercontent.com/poniitty/kilpisjarvi_microclimate/main/output/T4_data_daily.csv") %>% 
  mutate(site = gsub("RA","RAR",site),
         site = gsub("MI","SAA",site))

# Värriö
d3 <- read_csv("https://raw.githubusercontent.com/poniitty/varrio_microclimate/main/output/T4_data_daily.csv")

# Tiilikka
d4 <- read_csv("https://raw.githubusercontent.com/poniitty/Tiilikka_microclimates/main/output/T4_data_daily.csv")

# Pisa
d5 <- read_csv("https://raw.githubusercontent.com/poniitty/Pisa_microclimates/main/output/T4_data_daily.csv")

# Hyytiälä
d6 <- read_csv("https://raw.githubusercontent.com/poniitty/Hyytiala_microclimates/main/output/T4_data_daily.csv")

# Karkali
d7 <- read_csv("https://raw.githubusercontent.com/poniitty/Karkali_microclimate/main/output/T4_data_daily.csv")

# Combine all data and edit the sites ID's
bind_rows(d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> dailyT4

# Check
unique(dailyT4$area)

################################
# Combine all daily data
daily <- full_join(daily %>% rename(error_tomst = probl),
                   dailyT4 %>% rename(error_T4 = probl,
                                      logger_T4 = logger)) %>% 
  arrange(site, date)


daily %>% rename(id_code = site) %>% 
  relocate(starts_with("error"), .after = date) %>% 
  relocate(starts_with("T4_"), .after = T3_max) %>% 
  relocate(ends_with("_prop"), .after = date) %>% 
  relocate(logger_T4, .after = error_T4) -> daily

daily %>% mutate(site = parse_number(id_code)) %>% 
  relocate(site, .after = area) -> daily

# Daily data ready!
write_csv(daily, "data/all_data_daily.csv")


##################################################################################
# Monthly datasets
##################################################################################

# Download datasets
# Rastigaisa
d1 <- read_csv("https://raw.githubusercontent.com/poniitty/Rastigaisa_microclimate/main/output/tomst_data_monthly.csv")

# Kilpisjärvi
d2 <- read_csv("https://raw.githubusercontent.com/poniitty/kilpisjarvi_microclimate/main/output/tomst_data_monthly.csv") %>% 
  mutate(site = gsub("RA","RAR",site))

# Värriö
d3 <- read_csv("https://raw.githubusercontent.com/poniitty/varrio_microclimate/main/output/tomst_data_monthly.csv")

# Tiilikka
d4 <- read_csv("https://raw.githubusercontent.com/poniitty/Tiilikka_microclimates/main/output/tomst_data_monthly.csv")

# Pisa
d5 <- read_csv("https://raw.githubusercontent.com/poniitty/Pisa_microclimates/main/output/tomst_data_monthly.csv")

# Hyytiälä
d6 <- read_csv("https://raw.githubusercontent.com/poniitty/Hyytiala_microclimates/main/output/tomst_data_monthly.csv")

# Karkali
d7 <- read_csv("https://raw.githubusercontent.com/poniitty/Karkali_microclimate/main/output/tomst_data_monthly.csv")

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

# Combine all data and edit the sites ID's
bind_rows(d1 %>% mutate(site = paste0("RAS",unlist(lapply(site, add_zeros)))),
          d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> monthly

# Check
unique(monthly$area)

############################################################################
# haxo & hobo data

# Kilpisjärvi
d2 <- read_csv("https://raw.githubusercontent.com/poniitty/kilpisjarvi_microclimate/main/output/T4_data_monthly.csv") %>% 
  mutate(site = gsub("RA","RAR",site),
         site = gsub("MI","SAA",site))

# Värriö
d3 <- read_csv("https://raw.githubusercontent.com/poniitty/varrio_microclimate/main/output/T4_data_monthly.csv")

# Tiilikka
d4 <- read_csv("https://raw.githubusercontent.com/poniitty/Tiilikka_microclimates/main/output/T4_data_monthly.csv")

# Pisa
d5 <- read_csv("https://raw.githubusercontent.com/poniitty/Pisa_microclimates/main/output/T4_data_monthly.csv")

# Hyytiälä
d6 <- read_csv("https://raw.githubusercontent.com/poniitty/Hyytiala_microclimates/main/output/T4_data_monthly.csv")

# Karkali
d7 <- read_csv("https://raw.githubusercontent.com/poniitty/Karkali_microclimate/main/output/T4_data_monthly.csv")

# Combine all data and edit the sites ID's
bind_rows(d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> monthlyT4

# Check
unique(monthlyT4$area)

################################
# Combine all daily data
monthly <- full_join(monthly,
                     monthlyT4 %>% rename(logger_T4 = logger)) %>% 
  arrange(site, year, month)

names(monthly)
monthly %>% rename(id_code = site) %>% 
  relocate(starts_with("arh_"), .after = day_frac_moist) %>% 
  relocate(starts_with("moist_"), .after = day_frac_moist) %>% 
  relocate(starts_with("T4_"), .after = day_frac_moist) %>% 
  relocate(starts_with("T3_"), .after = day_frac_moist) %>% 
  relocate(starts_with("T2_"), .after = day_frac_moist) %>% 
  relocate(starts_with("T1_"), .after = day_frac_moist) %>% 
  relocate(starts_with("day_frac"), .after = month) %>% 
  relocate(day_frac_T4, .after = day_frac_T3) %>% 
  relocate(starts_with("logger_T4"), .after = month) -> monthly

monthly %>% rename(T1_prop = day_frac_T1,
                   T2_prop = day_frac_T2,
                   T3_prop = day_frac_T3,
                   T4_prop = day_frac_T4,
                   moist_prop = day_frac_moist,
                   arh_prop = day_frac_arh) -> monthly

monthly %>% mutate(site = parse_number(id_code)) %>% 
  relocate(site, .after = area) -> monthly

# Monthly data ready!
write_csv(monthly, "data/all_data_monthly.csv")

##############################################################################
# SNOW DATASETS
##############################################################################

# Download datasets
# Rastigaisa
d1 <- read_csv("https://raw.githubusercontent.com/poniitty/Rastigaisa_microclimate/main/output/snow_variables.csv")

# Kilpisjärvi
d2 <- read_csv("https://raw.githubusercontent.com/poniitty/kilpisjarvi_microclimate/main/output/snow_variables.csv") %>% 
  mutate(site = gsub("RA","RAR",site))

# Värriö
d3 <- read_csv("https://raw.githubusercontent.com/poniitty/varrio_microclimate/main/output/snow_variables.csv")

# Tiilikka
d4 <- read_csv("https://raw.githubusercontent.com/poniitty/Tiilikka_microclimates/main/output/snow_variables.csv")

# Pisa
d5 <- read_csv("https://raw.githubusercontent.com/poniitty/Pisa_microclimates/main/output/snow_variables.csv")

# Hyytiälä
d6 <- read_csv("https://raw.githubusercontent.com/poniitty/Hyytiala_microclimates/main/output/snow_variables.csv")

# Karkali
d7 <- read_csv("https://raw.githubusercontent.com/poniitty/Karkali_microclimate/main/output/snow_variables.csv")

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

# Combine all data and edit the sites ID's
bind_rows(d1 %>% mutate(site = paste0("RAS",unlist(lapply(site, add_zeros)))),
          d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> snow

# Check
unique(snow$area)

snow %>% rename(id_code = site) %>% 
  mutate(site = parse_number(id_code)) %>% 
  relocate(site, .after = area) -> snow


# Snow data ready!
write_csv(snow, "data/snow_vars_all.csv")



#######################################################################################
# GATHER THE RAW MICROCLIMATE DATA 
# The Tomst source datasets are not on Github due to file size limitations
# If needed contact Pekka Niittynen to get access

# Download datasets
# Rastigaisa
d1 <- read_csv("../Rastigaisa_microclimate/output/tomst_data.csv")

# Kilpisjärvi
d2 <- read_csv("../kilpisjarvi_microclimate/output/tomst_data.csv") %>% 
  mutate(site = gsub("RA","RAR",site))

# Värriö
d3 <- read_csv("../varrio_microclimate/output/tomst_data.csv")

# Tiilikka
d4 <- read_csv("../Tiilikka_microclimates/output/tomst_data.csv")

# Pisa
d5 <- read_csv("../Pisa_microclimates/output/tomst_data.csv")

# Hyytiälä
d6 <- read_csv("../Hyytiala_microclimates/output/tomst_data.csv")

# Karkali
d7 <- read_csv("../Karkali_microclimate/output/tomst_data.csv")

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

# Combine all data and edit the sites ID's
bind_rows(d1 %>% mutate(site = paste0("RAS",unlist(lapply(site, add_zeros)))),
          d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> all

# Check
unique(all$area)

# Calibration function for the moisture count values for unknown soils from Kopecky et al. 2020
cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }

# Calibrate the moisture values
all %>% rename(moist_count = moist) %>% 
  mutate(moist = round(cal_funNA(moist_count),1)) %>% 
  mutate(moist = ifelse(T1 < 1 | probl == 1, NA, moist)) -> all

all %>% relocate(site, area) %>% 
  relocate(moist, .after = T3) -> all

############################################################################
# haxo & hobo data

# Kilpisjärvi
d2 <- read_csv("../kilpisjarvi_microclimate/output/T4_combined.csv") %>% 
  mutate(site = gsub("RA","RAR",site),
         site = gsub("MI","SAA",site))

# Värriö
d3 <- read_csv("../varrio_microclimate/output/haxo_data_corrected.csv") %>% 
  mutate(logger = "a") %>% 
  rename(probl = haxo_probl)

# Tiilikka
d4 <- read_csv("../Tiilikka_microclimates/output/haxo_data_corrected.csv") %>% 
  mutate(logger = "a") %>% 
  rename(probl = haxo_probl)

# Pisa
d5 <- read_csv("../Pisa_microclimates/output/haxo_data_corrected.csv") %>% 
  mutate(logger = "a") %>% 
  rename(probl = haxo_probl)

# Hyytiälä
d6 <- read_csv("../Hyytiala_microclimates/output/haxo_data_corrected.csv") %>% 
  mutate(logger = "a") %>% 
  rename(probl = haxo_probl)

# Karkali
d7 <- read_csv("../Karkali_microclimate/output/haxo_data_corrected.csv") %>% 
  mutate(logger = "a") %>% 
  rename(probl = haxo_probl)

# Combine all data and edit the sites ID's
bind_rows(d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> allT4

# Check
unique(allT4$area)

################################
# Combine all daily data
all <- full_join(all %>% rename(error_tomst = probl),
                 allT4 %>% rename(error_T4 = probl,
                                  logger_T4 = logger)) %>% 
  arrange(site, datetime)


all %>% rename(id_code = site,
               T4 = at) %>% 
  relocate(starts_with("error"), .after = datetime) %>% 
  relocate(T4, .after = T3) %>%
  relocate(logger_T4, .after = error_T4) -> all

all %>% mutate(site = parse_number(id_code)) %>% 
  relocate(site, .after = area) -> all

# Daily data ready!
fwrite(all, "C:/datacloud/biogeoclimate/microclimate/data/logger/all_data.csv")
# fread("C:/datacloud/biogeoclimate/microclimate/data/logger/all_data.csv")
