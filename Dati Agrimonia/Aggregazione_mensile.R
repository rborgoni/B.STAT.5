#############################################################################
########## Aggregazione dataset AgrImOnIA da giornaliero a mensile ##########
#############################################################################

library(tidyverse)
library(sf)
library(lubridate)

setwd("~/GitHub/B.STAT.5/Dati Agrimonia")

##### Upload dati giornalieri
load("~/GitHub/B.STAT.5/Dati Agrimonia/Agrimonia_Giornaliero.RData")
str(Agrimonia)

##### Aggregazione mensile
Agrimonia_month <- Agrimonia %>%
  mutate(Year = year(Time), Month = month(Time)) %>%
  group_by(IDStation,NameStation,Altitude,Province,Latitude,Longitude,ARPA_zone,ARPA_stat_type,
           LAU_Code,GISCO_ID,CNTR_CODE,LAU_NAME,POP_2020,POP_DENS_2,AREA_KM2,FID,
           NUTS0_Code,NUTS0_Name,NUTS1_Code,NUTS1_Name,NUTS2_Code,NUTS2_Name,
           NUTS3_Code,NUTS3_Name,NUTS3_Border,NUTS3_UrbRur,NUTS3_Remoteness,NUTS3_Metropol,
           NUTS3_Coastal,NUTS3_Mountain,
           Year,Month) %>%
  summarise(across(contains("AQ_"), function(x) mean(x,na.rm=T)),
            across(contains("LI_"), function(x) mean(x,na.rm=T)),
            # across(contains("LA_"), function(x) mean(x,na.rm=T)),
            across(contains("EM_"), function(x) sum(x,na.rm=T)),
            across(c(WE_blh_layer_max,WE_blh_layer_min,
                     WE_rh_max,WE_rh_mean,WE_rh_min,
                     WE_solar_radiation,WE_surface_pressure, WE_temp_2m,
                     WE_wind_speed_100m_max,WE_wind_speed_100m_mean,
                     WE_wind_speed_10m_max,WE_wind_speed_10m_mean), function(x) mean(x,na.rm=T)),
            across(c(WE_tot_precipitation), function(x) sum(x,na.rm=T))) %>%
  mutate(Time = lubridate::make_date(year = Year, month = Month), .after = "Month")

save(Agrimonia_month, file = "Agrimonia_Mensile.RData")
