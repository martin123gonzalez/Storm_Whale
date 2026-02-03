#Title: Whale And Storm 
#Author: Martin A Gonzalez
#BTBEL, UCSC

# concise version of "Storm_and_Whale_concise_for_Supplement.R" 
# with only statistics for Ari export.


#----Packages----
library(tidyverse)
library(here)

#----Data Import----
whale_storm_dummy <- read.csv(here::here("Output_Data", "whale_storm_data_export_ari_20260202.csv"))

##----Time series data creation----
###----Storm----
storm_distinct2015 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2015) %>% 
  dplyr::filter(Julian_new >= 4 & Julian_new <= 33) %>% 
  select("Avg_CVU_per_day")

storm_distinct2016 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2016) %>% 
  dplyr::filter(Julian_new >= 16 & Julian_new <= 99) %>% 
  select("Avg_CVU_per_day")

storm_distinct2017 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2017) %>% 
  dplyr::filter(Julian_new >= 5 & Julian_new <= 116) %>% 
  select("Avg_CVU_per_day")

storm_distinct2018 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2018) %>% 
  dplyr::filter(Julian_new >= 5 & Julian_new <= 93) %>% 
  select("Avg_CVU_per_day")

storm_distinct2019 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2019) %>% 
  dplyr::filter(Julian_new >= 8 & Julian_new <= 77) %>% 
  select("Avg_CVU_per_day")

storm_distinct2020 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Julian_new >= 3 & Julian_new <= 78) %>% 
  select("Avg_CVU_per_day")

storm_2015_ts = ts(as.vector(ts(storm_distinct2015)))
storm_2016_ts = ts(as.vector(ts(storm_distinct2016)))
# storm_2016_ts = as.vector(ts(storm_distinct2016))
# storm_2016_ts = ts(storm_distinct2016)
storm_2017_ts = ts(as.vector(ts(storm_distinct2017)))
storm_2018_ts = ts(as.vector(ts(storm_distinct2018)))
storm_2019_ts = ts(as.vector(ts(storm_distinct2019)))
storm_2020_ts = ts(as.vector(ts(storm_distinct2020)))

###----Whale----
whale_distinct2015 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2015) %>% 
  dplyr::filter(Julian_new >= 4 & Julian_new <= 33) %>% 
  select("Whales_per_day_effort_correct")
# 
# whale_test2015 <- whale_storm_dummy %>% 
#   dplyr::filter(Year == 2015) 

whale_distinct2016 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2016) %>% 
  dplyr::filter(Julian_new >= 16 & Julian_new <= 99) %>% 
  select("Whales_per_day_effort_correct")

whale_distinct2017 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2017) %>% 
  dplyr::filter(Julian_new >= 5 & Julian_new <= 116) %>% 
  select("Whales_per_day_effort_correct")

whale_distinct2018 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2018) %>% 
  dplyr::filter(Julian_new >= 5 & Julian_new <= 93) %>% 
  select("Whales_per_day_effort_correct")

whale_distinct2019 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2019) %>% 
  dplyr::filter(Julian_new >= 8 & Julian_new <= 77) %>% 
  select("Whales_per_day_effort_correct")

whale_distinct2019_MAG_edit <- whale_storm_dummy %>% #changes 40 CPUE to month average
  dplyr::filter(Year == 2019) %>% 
  dplyr::filter(Julian_new >= 8 & Julian_new <= 77) %>% 
  dplyr::group_by(Month) %>% 
  dplyr::mutate(Avg_whale_per_month = mean(Whales_per_day_effort_correct)) %>% 
  ungroup()

whale_distinct2019_MAG_edit[11, "Whales_per_day_effort_correct"] <- 2.0223062
whale_distinct2019_MAG_edit[38, "Whales_per_day_effort_correct"] <- 2.5051125
whale_distinct2019_MAG_edit[40, "Whales_per_day_effort_correct"] <- 2.5051125

whale_distinct2019_MAG_edit <- whale_distinct2019_MAG_edit %>%  
  select("Whales_per_day_effort_correct")

whale_distinct2020 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Julian_new >= 3 & Julian_new <= 78) %>% 
  select("Whales_per_day_effort_correct")

whale_2015_ts = ts(as.vector(ts(whale_distinct2015)))
whale_2016_ts = ts(as.vector(ts(whale_distinct2016)))
whale_2017_ts = ts(as.vector(ts(whale_distinct2017)))
whale_2018_ts = ts(as.vector(ts(whale_distinct2018)))
whale_2019_ts = ts(as.vector(ts(whale_distinct2019)))
whale_2020_ts = ts(as.vector(ts(whale_distinct2020)))
#----Whale Storm----
whale_storm_distinct2015 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2015) %>% 
  dplyr::filter(Julian_new >= 4 & Julian_new <= 33) 

whale_storm_distinct2016 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2016) %>% 
  dplyr::filter(Julian_new >= 16 & Julian_new <= 99) 

whale_storm_distinct2017 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2017) %>% 
  dplyr::filter(Julian_new >= 5 & Julian_new <= 116) 

whale_storm_distinct2018 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2018) %>% 
  dplyr::filter(Julian_new >= 5 & Julian_new <= 93) 

whale_storm_distinct2019 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2019) %>% 
  dplyr::filter(Julian_new >= 8 & Julian_new <= 77) 

whale_storm_distinct2020 <- whale_storm_dummy %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Julian_new >= 3 & Julian_new <= 78) 



##----Time Lag----      
#can also use storm_20xx_ts and whale_20xx_ts below
whale_distinct2016_test2 <- whale_distinct2016 %>% 
  dplyr::mutate(Whales_per_day_effort_correct = ifelse(Whales_per_day_effort_correct == 0, NA, Whales_per_day_effort_correct))

ccf_2016 <- ccf(x = storm_distinct2016, y = whale_distinct2016_test2, type = "correlation")
ccf_2016      #sig @ time lag = -10, -11, -12, -13
png("Figures/Time-Lag/ccf_2016.png", width = 857, height = 534)
plot(ccf_2016, main = "Average daily storm CVU vs total Humpback whale SPUE in 2016")
dev.off()





####----fig 2; 2015 time lag----
ccf_2015 <- ccf(x = storm_distinct2015, y = whale_distinct2015, type = "correlation")
ccf_2015      #sig @ time lag = none (not significant)
png("Figures/Time-Lag/ccf_2015.png", width = 857, height = 534)
plot(ccf_2015, main = "Average daily storm CVU vs total Humpback whale SPUE in 2015")
dev.off()

####----fig 3; 2016 time lag----
ccf_2016 <- ccf(x = storm_distinct2016, y = whale_distinct2016, type = "correlation")
ccf_2016      #sig @ time lag = -10, -11, -12, -13
png("Figures/Time-Lag/ccf_2016.png", width = 857, height = 534)
plot(ccf_2016, main = "Average daily storm CVU vs total Humpback whale SPUE in 2016")
dev.off()

ccf_2016 <- ccf(x = storm_distinct2016, y = whale_distinct2016, type = "correlation")
ccf_2016      #sig @ time lag = -10, -11, -12, -13
png("Figures/Time-Lag/ccf_2016_panel.png", width = 857, height = 534)
plot(ccf_2016, main = "2016")
dev.off()

#switch test, ignore
# ccf_2016 <- ccf(x = whale_distinct2016, y = storm_distinct2016, type = "correlation")
# ccf_2016      #sig @ time lag = -10, -11, -12, -13
# plot(ccf_2016, main = "Average daily storm CVU vs total Humpback whale SPUE in 2016")


####----fig 4; 2017 time lag----
ccf_2017 <- ccf(x = storm_distinct2017, y = whale_distinct2017, type = "correlation")
ccf_2017      #sig @ time lag = -12
png("Figures/Time-Lag/ccf_2017.png", width = 857, height = 534)
plot(ccf_2017, main = "Average daily storm CVU vs total Humpback whale SPUE in 2017")
dev.off()

ccf_2017 <- ccf(x = storm_distinct2017, y = whale_distinct2017, type = "correlation")
ccf_2017      #sig @ time lag = -12
png("Figures/Time-Lag/ccf_2017_panel.png", width = 857, height = 534)
plot(ccf_2017, main = "2017")
dev.off()

####----fig 5; 2018 time lag----
ccf_2018 <- ccf(x = storm_distinct2018, y = whale_distinct2018, type = "correlation")
ccf_2018      #sig @ time lag = -4
png("Figures/Time-Lag/ccf_2018.png", width = 857, height = 534)
plot(ccf_2018, main = "Average daily storm CVU vs total Humpback whale SPUE in 2018")
dev.off()

ccf_2018 <- ccf(x = storm_distinct2018, y = whale_distinct2018, type = "correlation")
ccf_2018      #sig @ time lag = -4
png("Figures/Time-Lag/ccf_2018_panel.png", width = 857, height = 534)
plot(ccf_2018, main = "2018")
dev.off()

####----fig 6; 2019 time lag ----
ccf_2019 <- ccf(x = storm_distinct2019, y = whale_distinct2019, type = "correlation")
ccf_2019      #sig @ time lag = none (not significant)
png("Figures/Time-Lag/ccf_2019.png", width = 857, height = 534)
plot(ccf_2019, main = "Average daily storm CVU vs total Humpback whale SPUE in 2019")
dev.off()

####----fig 7; 2020 time lag ----
ccf_2020 <- ccf(x = storm_distinct2020, y = whale_distinct2020, type = "correlation")
ccf_2020      #sig @ time lag = +10
png("Figures/Time-Lag/ccf_2020_panel.png", width = 857, height = 534)
plot(ccf_2020, main = "2020")
dev.off()

ccf_2020 <- ccf(x = storm_distinct2020, y = whale_distinct2020, type = "correlation")
ccf_2020      #sig @ time lag = +10
png("Figures/Time-Lag/ccf_2020.png", width = 857, height = 534)
plot(ccf_2020, main = "Average daily storm CVU vs total Humpback whale SPUE in 2020")
dev.off()