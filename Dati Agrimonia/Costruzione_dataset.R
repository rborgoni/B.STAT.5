#############################################################################
########## Attività PNRR - Liceo Lussana di Bergamo (Febbraio 2025 ##########
##########      Filtraggio dataset AgrImOnIA da giornaliero        ##########
#############################################################################

library(ggplot2)
library(tidyverse)
library(readr)

##### Data source
setwd("~/GitHub/B.STAT.5/Dati Agrimonia")

##### Upload dati giornalieri
load("Agrimonia_Giornaliero.RData")
str(Agrimonia)

Agrimonia <- Agrimonia %>%
  mutate(
    Year = year(Time),
    Month = month(Time),
    Day = day(Time),
    .after = Time)  %>%
  select(
    IDStation,NameStation,Altitude,Latitude,Longitude,ARPA_zone,ARPA_stat_type,
    LAU_Code,LAU_NAME,POP_2020,POP_DENS_2,AREA_KM2,
    NUTS0_Code,NUTS0_Name,NUTS1_Code,NUTS1_Name,NUTS2_Code,NUTS2_Name,
    NUTS3_Code,NUTS3_Name,NUTS3_Border,NUTS3_UrbRur,NUTS3_Remoteness,NUTS3_Metropol,
    NUTS3_Coastal,NUTS3_Mountain,
    Time,Year,Month,Day,
    AQ_pm10,
    AQ_pm25,
    AQ_nh3,
    WE_temp_2m,
    WE_wind_speed_10m_mean,
    WE_wind_speed_10m_max,
    WE_tot_precipitation,
    WE_blh_layer_max,
    WE_blh_layer_min,
    LI_pigs = LI_pigs_v2,
    LI_bovine = LI_bovine_v2,
    LA_hvi,
    LA_lvi
  )
  
save(Agrimonia, file = "Agrimonia_Giornaliero_Ridotto.RData")
  