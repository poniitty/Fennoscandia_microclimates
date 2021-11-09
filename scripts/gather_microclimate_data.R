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

# Daily data ready!
write_csv(daily, "data/daily_data_all.csv")


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

# Monthly data ready!
write_csv(monthly, "data/monthly_data_all.csv")

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

# Snow data ready!
write_csv(snow, "data/snow_vars_all.csv")

