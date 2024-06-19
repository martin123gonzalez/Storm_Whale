#Title: Whale And Storm 
#Author: Martin A Gonzalez
#BTBEL, UCSC

# average quick note
(-4+-10+-12)/3 #= -8.667

(-4+-10+-12+-12+-11+-13)/6 #= -10.333


#----Packages----
library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(ggplot2)
library(maps)
# install.packages('ggmap')
library(ggmap)
library(marmap)
library(mapproj)
library(nlme)
library(bbmle)
# install.packages("astsa")
library(astsa)
library(plyr);
library(dplyr)
library(pscl)

#----Functions----

## BELOW Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#----Data Inport----
storm_dirty <- read.csv(here::here("Input_Data", "track_list_19792021_ERA5_SH.csv"))
whale_sightings_dirty <- read.csv(here::here("Input_Data", "BTBEL_LTER_Sightings_Database_Manual_Edits_alteration.csv"))
whale_effort_dirty <- read.csv(here::here("Input_Data", "BTBEL_LTER_Effort_Log_Database_20152023.csv"))

storm_clean_PAL_LTER_dummy <- read.csv(here::here("Output_Data", "storm_PAL_LTER_dummy.csv")) 
                        #Note: This is storm_PAL_LTER.csv but with dummy 0 values to create a true time-series

pen_1819 <- read.csv(here::here("Input_Data", "Penguin_Team_PAL_1819.csv"))
pen_1920 <- read.csv(here::here("Input_Data", "Penguin_Team_PAL_1920.csv"))
pen_2021 <- read.csv(here::here("Input_Data", "Penguin_Team_PAL_2021.csv"))
#----Cleaning----

##----Storm Data Cleaning----
#clean data, SouthWestern Hemisphere
colnames(storm_dirty) <- c("ID_Number", "Num_6_hr_periods", "Year", "Month", 
                           "Day", "Hour", "Longitude", "Latitude", "Intensity_CVU")
storm_clean <- storm_dirty %>% 
  select(-"ID_Number", -"Num_6_hr_periods") %>% 
  dplyr::filter(Year >= 2015 & Year <=2020) %>% 
  dplyr::filter(Month >= 1 & Month <=4) %>% 
  dplyr::filter(Latitude < -50) %>% 
  dplyr::filter(Longitude > 180) %>% 
  dplyr::mutate(Longitude = Longitude - 360) 

storm_PAL_LTER <- storm_clean %>% 
  dplyr::filter(Latitude <= -63 & Latitude >= -70) %>% 
  dplyr::filter(Longitude <= -61 & Longitude >= -80) 

#export csv to create dummy values from filtered CVU
storm_PAL_LTER_dummy <- storm_PAL_LTER %>% 
  dplyr::mutate(Day_char = as.character(Day)) %>% 
  dplyr::mutate(Month_char = as.character(Month)) %>% 
  dplyr::mutate(Year_char = as.character(Year)) %>% 
  dplyr::mutate(MDY = str_c(Month_char, Day_char, Year_char, sep = "/")) %>% 
  dplyr::mutate(Julian_new = as.Date(MDY, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(Julian_new = yday(Julian_new))

#storm data in PAL LTER all CVU levels                        ####THIS ONE
storm_clean_PAL_LTER_allCVU <- storm_clean_PAL_LTER_dummy %>% 
  # dplyr::filter(Intensity_CVU >= 3) %>%
  dplyr::group_by(Year, Month, Day) %>% 
  dplyr::mutate(Avg_CVU_per_day = mean(Intensity_CVU)) %>% 
  # group_by(Year, Month) %>% 
  # mutate(Avg_CVU_per_month = mean(Intensity_CVU)) %>% 
  # group_by(Year) %>% 
  # mutate(Avg_CVU_per_year = mean(Intensity_CVU)) %>% 
  # ungroup() %>% 
  dplyr::mutate(Strong_CVU = ifelse(Avg_CVU_per_day >=3, "yes", "no")) %>% 
  dplyr::mutate(Day_char = as.character(Day)) %>% 
  dplyr::mutate(Month_char = as.character(Month)) %>% 
  dplyr::mutate(Year_char = as.character(Year)) %>% 
  dplyr::mutate(MDY = str_c(Month_char, Day_char, Year_char, sep = "/")) %>% 
  dplyr::mutate(Julian_new = as.Date(MDY, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(Julian_new = yday(Julian_new))

#storm data in PAL LTER greater than 3 CVU
storm_clean_PAL_LTER_morethan3CVU <- storm_clean_PAL_LTER_dummy%>% 
  dplyr::filter(Intensity_CVU >= 3) %>%
  dplyr::group_by(Year, Month, Day) %>% 
  dplyr::mutate(Avg_strong_CVU_per_day = mean(Intensity_CVU)) %>% 
  # group_by(Year, Month) %>% 
  # mutate(Avg_strong_CVU_per_month = mean(Intensity_CVU)) %>% 
  # group_by(Year) %>% 
  # mutate(Avg_strong_CVU_per_year = mean(Intensity_CVU)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Day_char = as.character(Day)) %>% 
  dplyr::mutate(Month_char = as.character(Month)) %>% 
  dplyr::mutate(Year_char = as.character(Year)) %>% 
  dplyr::mutate(MDY = str_c(Month_char, Day_char, Year_char, sep = "/")) %>% 
  dplyr::mutate(Julian_new = as.Date(MDY, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(Julian_new = yday(Julian_new))

#distinct storm data
# storm_strong_distinct <- storm_clean_PAL_LTER_morethan3CVU %>% 
#   select(Julian_new, Year, Month, Day, Avg_strong_CVU_per_day) %>% 
#   distinct()

storm_all_distinct <- storm_clean_PAL_LTER_allCVU %>%
  select(Julian_new, Year, Month, Day, Avg_CVU_per_day) %>%
  distinct() %>% 
  ungroup()


test_all_distinct <- storm_clean_PAL_LTER_allCVU %>%
  select(Julian_new, Year, Month, Day, Avg_CVU_per_day, Length_day_storm, Event) %>%
  distinct() %>% 
  ungroup()

test_event_temp <- test_all_distinct %>% 
  dplyr::filter(Event != "") %>% 
  dplyr::mutate(During_FieldSeason = ifelse(Year == 2015 & Julian_new <= 33 & Julian_new >= 4, "Yes2015",
                                     ifelse(Year == 2016 & Julian_new <= 99 & Julian_new >= 16, "Yes2016", 
                                     ifelse(Year == 2017 & Julian_new <= 116 & Julian_new >= 5, "Yes2017", 
                                     ifelse(Year == 2018 & Julian_new <= 93 & Julian_new >= 4, "Yes2018", 
                                     ifelse(Year == 2019 & Julian_new <= 77 & Julian_new >= 8, "Yes2019",
                                     ifelse(Year == 2020 & Julian_new <= 78 & Julian_new >=3, "Yes2020", "No"))))))) %>% 
  dplyr::distinct(Event, Length_day_storm, Year, During_FieldSeason) 

test_event_summary <- test_event_temp %>%
  dplyr::filter(During_FieldSeason != "No") %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(Storm_event_per_year = n())
    
  
storm_event_distinct <- storm_clean_PAL_LTER_allCVU %>% 
  ungroup() %>% 
  select(Event, Length_day_storm, Days_since_last_storm) %>% 
  distinct()


storm_real_distinct <- storm_clean_PAL_LTER_allCVU %>% 
  dplyr::filter(Avg_CVU_per_day > 1)

#print CSV
write_csv(x = storm_PAL_LTER,
          file = here::here("Output_Data", "storm_PAL_LTER.csv"))



##----Whale Sighting Cleaning----
whale_sight_clean <- whale_sightings_dirty %>% 
  dplyr::filter(Species == "Mn") %>% 
  separate(Datetime, c('Date', 'Time'), sep = " ") %>%
  mutate(Julian = Date) %>% 
  separate(Date, c('Month', 'Day', 'Year'), sep = "/") %>%
  select(-Calves, -Comments, -Whales_per_Trip) %>% 
  dplyr::filter(Longitude != '') %>% 
  dplyr::filter(Trip_Duration_min != "NA")

# whale_sight_clean$Year = paste(20, whale_sight_clean$Year, sep = "")

whale_sight_clean <- transform(whale_sight_clean,
                               Day = as.numeric(Day),
                               Month = as.numeric(Month),
                               Year = as.numeric(Year))

whale_sight_clean <- whale_sight_clean %>% 
  dplyr::filter(Year != 2023) #%>% 
  # dplyr::filter(Group.Size < 30)
#LOOK HERE FOR CHANGED TO WHALE_SIGHT_CLEAN_2019


whale_sight_clean <- whale_sight_clean[order(whale_sight_clean$Year, whale_sight_clean$Month, whale_sight_clean$Day),]

whale_sight_clean <- whale_sight_clean %>% 
  dplyr::group_by(Year, Month, Day) %>% 
  dplyr::mutate(Whales_per_day = sum(Group.Size)) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::mutate(Whales_per_month = sum(Group.Size)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::mutate(Whales_per_year = sum(Group.Size)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Day_Duration_hour = Day_Duration_min / 60) %>% 
  dplyr::mutate(Whales_per_day_effort_correct = Whales_per_day / Day_Duration_hour)

whale_sight_clean <- whale_sight_clean %>% 
  dplyr::mutate(Julian_new = as.Date(Julian, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(Julian_new = yday(Julian_new)) %>% 
  dplyr::mutate(Year_char = as.character(Year))

whale_sight_clean <- whale_sight_clean[, c(14, 18, 3, 1, 2, 5, 6, 7, 8, 9, 17, 
                                           15, 16, 10, 11, 12, 4,  19, 13)]

whale_sight_clean <- whale_sight_clean %>% 
  dplyr::filter(Whales_per_day_effort_correct > 0)# %>% 
  # group_by(Month) %>% 
  # mutate(Whales_per_month_effort_correct = sum(Whales_per_day_effort_correct))


#print csv
write_csv(x = whale_sight_clean,
          file = here::here("Output_Data", "whale_sight_clean.csv"))




##----Whale Effort Cleaning----
whale_effort_clean <- whale_effort_dirty %>% 
  dplyr::filter(Duration != "" & Duration != "0:00") %>% 
  separate(Date, c('Month', 'Day', 'Year'), sep = "/") %>% 
  separate(Duration, c('Hour', 'Minutes'), sep = ":")
  
whale_effort_clean <- transform(whale_effort_clean,
                               Day = as.numeric(Day),
                               Month = as.numeric(Month),
                               Year = as.numeric(Year),
                               Hour = as.numeric(Hour),
                               Minutes = as.numeric(Minutes))

whale_effort_clean <- whale_effort_clean %>% 
  dplyr::filter(Year != 2023) %>% 
  dplyr::mutate(Duration_min = (60 * Hour) + Minutes) %>%
  dplyr::group_by(Year, Month, Day) %>% 
  dplyr::mutate(Day_Duration = sum(Duration_min))

whale_effort_clean <- whale_effort_clean[, c(3, 1, 2, 4, 5, 6, 7, 10, 11)]

whale_effort_clean <- whale_effort_clean[order(whale_effort_clean$Year, whale_effort_clean$Month, whale_effort_clean$Day),]

#print csv
write_csv(x = whale_effort_clean,
          file = here::here("Output_Data", "whale_effort_clean.csv"))

##----Whale Sighting, num whales per day, effort corrected, no Lat-Long
whale_sight_distinct <- whale_sight_clean %>% 
  select(Julian_new, Year, Month, Day, Species, Whales_per_day_effort_correct) %>% 
  distinct() %>% 
  ungroup()


##----Whale-Storm Joint Data Frame----
###NOTE: this is NOT effort corrected data
# whale_temp <- whale_sight_distinct


# whale_storm_strongCVU <- full_join(whale_sight_distinct, storm_strong_distinct, 
                                   # by = c("Julian_new", "Year", "Month", "Day"))

whale_storm_allCVU <- full_join(whale_sight_distinct, storm_all_distinct, 
                                by = c("Julian_new", "Year", "Month", "Day"))

whale_storm_allCVU <- whale_storm_allCVU %>% 
  dplyr::mutate(During_FieldSeason = ifelse(Year == 2015 & Julian_new <= 33 & Julian_new >= 4, "Yes2015",
                                            ifelse(Year == 2016 & Julian_new <= 99 & Julian_new >= 16, "Yes2016", 
                                            ifelse(Year == 2017 & Julian_new <= 116 & Julian_new >= 5, "Yes2017", 
                                            ifelse(Year == 2018 & Julian_new <= 93 & Julian_new >= 4, "Yes2018", 
                                            ifelse(Year == 2019 & Julian_new <= 77 & Julian_new >= 8, "Yes2019",
                                            ifelse(Year == 2020 & Julian_new <= 78 & Julian_new >=3, "Yes2020", "No")))))))

whale_storm_dummy <- whale_storm_allCVU %>% 
  select(-"Species") 

whale_storm_dummy[is.na(whale_storm_dummy)] <- 0
  
whale_storm_dummy <- whale_storm_dummy[order(whale_storm_dummy$Year, whale_storm_dummy$Julian_new), ]

# #All CVU values

##----Penguin Team Data----
pen_whale_dirty <- full_join(pen_1819, pen_1920)
pen_whale_dirty2 <- full_join(pen_whale_dirty, pen_2021)

pen_whale_clean <- pen_whale_dirty2 %>% 
  select(-X) %>% 
  dplyr::filter(Species == "HUWH") %>% 
  dplyr::filter(Transect == "Gentoo" | Transect == "Adelie") %>% 
  dplyr::filter(Count != "") %>% 
  dplyr::mutate(Julian = Date) %>% 
  separate(Date, c('Month', 'Day', 'Year'), sep = "/") %>% 
  dplyr::mutate(Julian_new = as.Date(Julian, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(Julian_new = yday(Julian_new)) %>% 
  dplyr::mutate(Year_char = as.character(Year)) %>% 
  dplyr::mutate(Month = as.numeric(Month)) %>% 
  dplyr::mutate(Day = as.numeric(Day)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>% 
  dplyr::filter(Year == 2019 | Year == 2020) %>% 
  dplyr::filter(Julian_new < 300) %>% 
  dplyr::mutate(Julian_bin = cut(Julian_new, breaks = c(0, 7, 14, 21, 28, 35, 42, 49, 
                                                 56, 63, 70, 77, 84))) %>% 
  dplyr::group_by(Year, Julian_bin) %>%
  dplyr::mutate(Week_whale_avg = mean(Count)) %>% 
  # ungroup() %>% 
  dplyr::group_by(Transect, Year, Julian_bin) %>% 
  dplyr::mutate(Transect_whale_week_avg = mean(Count))
  
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

#----Whale_storm Summary Graphs----
whale_storm_summary <- whale_storm_dummy %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>%
  dplyr::summarize(mean_whales = mean(Whales_per_day_effort_correct), avg_CVU = mean(Avg_CVU_per_day))

ggplot(data = whale_storm_summary, aes(x = mean_whales, y = avg_CVU, color = as.character(Year))) + 
  geom_point(size = 3) +
  labs(title = "avg daily whale vs avg cvu ")

cor.test(~mean_whales + avg_CVU, data = whale_storm_summary)

##----SPUE vs number stormy days----
###----total SPUE per year vs number stormy days or # events----
whale_sum_SPUE_half <- whale_storm_dummy %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(sum_whale_SPUE = sum(Whales_per_day_effort_correct))

#by all stormy days
whale_sum_SPUE_whole_all_days <- full_join(whale_sum_SPUE_half, whale_num_storm_summary_storm_all_half, by = "Year")  

ggplot(data = whale_sum_SPUE_whole_all_days, aes(x = num_stormy_days_all_CVU, y = sum_whale_SPUE, color = as.character(Year))) + 
  geom_point(size = 3) + 
  labs(title = "Number of stormy days vs total whale SPUE in each year",
       x = "Number of stormy days", y = "Total whale SPUE", color = "Year")
ggsave(here::here("Figures", "stormy_daysvs_total_SPUE_per_year.png"), width = 7.1, height = 4.2)

cor.test(~num_stormy_days_all_CVU + sum_whale_SPUE, data = whale_sum_SPUE_whole_all_days)


total_SPUE_vs_num_stormy_days <- lm(sum_whale_SPUE ~ num_stormy_days_all_CVU, data = whale_sum_SPUE_whole_all_days)
summary(total_SPUE_vs_num_stormy_days)

#TOTAL WHALES PER YEAR by strong stormy days
whale_sum_SPUE_whole_strong_days <- full_join(whale_sum_SPUE_half, whale_num_storm_summary_storm_strong_half, by = "Year")  

ggplot(data = whale_sum_SPUE_whole_strong_days, aes(x = num_stormy_days_strong_CVU, y = sum_whale_SPUE, color = as.character(Year))) + 
  geom_point(size = 3) + 
  labs(title = "Number of stormy days vs total whale SPUE in each year",
       x = "Number of days with a storm per year", y = "Total whale SPUE per year", color = "Year")
ggsave(here::here("Figures", "strong_stormy_daysvs_total_SPUE_per_year.png"), width = 7.1, height = 4.2)

total_SPUE_vs_num_strong_stormy_days <- lm(sum_whale_SPUE ~ num_stormy_days_strong_CVU, data = whale_sum_SPUE_whole_strong_days)
summary(total_SPUE_vs_num_strong_stormy_days)

cor.test(~num_stormy_days_strong_CVU + sum_whale_SPUE, data = whale_sum_SPUE_whole_strong_days)

#by number of events
whale_sum_SPUE_whole_all_events <- full_join(whale_sum_SPUE_half, whale_num_storm_summary_storm_all_half_beta, by = "Year")  

ggplot(data = whale_sum_SPUE_whole_all_events, aes(x = Storm_event_per_year, y = sum_whale_SPUE, color = as.character(Year))) + 
  geom_point(size = 3)
cor.test(~num_stormy_days_strong_CVU + sum_whale_SPUE, data = whale_sum_SPUE_whole_strong_days)


###----storm_real_distinct for length of events and number events; whale_storm_dummy for SPUE----
whale_num_storm_summary_storm_all_half_beta <- test_event_summary

whale_num_storm_summary_all <- full_join(whale_num_storm_summary_whale_half, whale_num_storm_summary_storm_all_half_beta, by = "Year")

ggplot(data = whale_num_storm_summary_all, aes(x = Storm_event_per_year, y = mean_whales_per_day, color = as.character(Year))) + 
  geom_point(size = 3) 
  # labs(title = "number of strong stormy (cvu >3) days per year vs avg whale SPUE", y = "number of strong stormy days per year")
cor.test(~Storm_event_per_year + mean_whales_per_day, data = whale_num_storm_summary_all)


###----whale_storm_dummy----
whale_num_storm_summary_whale_half <- whale_storm_dummy %>% 
  select(-Avg_CVU_per_day) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>% 
  dplyr::summarize(mean_whales_per_day = mean(Whales_per_day_effort_correct))

whale_num_storm_summary_storm_all_half <- whale_storm_dummy %>% 
  select(-Whales_per_day_effort_correct) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>% 
  dplyr::filter(Avg_CVU_per_day > 1) %>% 
  dplyr::summarize(num_stormy_days_all_CVU = n())

whale_num_storm_summary_storm_strong_half <- whale_storm_dummy %>% 
  select(-Whales_per_day_effort_correct) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>% 
  dplyr::filter(Avg_CVU_per_day > 3) %>% 
  dplyr::summarize(num_stormy_days_strong_CVU = n())

whale_num_storm_all_summary <- full_join(whale_num_storm_summary_whale_half, whale_num_storm_summary_storm_all_half, by = "Year")
whale_num_storm_strong_summary <- full_join(whale_num_storm_summary_whale_half, whale_num_storm_summary_storm_strong_half, by = "Year")

#all cvu
ggplot(data = whale_num_storm_all_summary, aes(x = num_stormy_days_all_CVU, y = mean_whales_per_day, color = as.character(Year))) + 
  geom_point(size = 3) +
  labs(title = "Number of stormy days vs average whale SPUE in each year", 
       x = "Number of stormy days", y = "Average daily whale SPUE", color = "Year")
ggsave(here::here("Figures", "stormy_daysvs_avg_SPUE_per_year.png"), width = 7.1, height = 4.2)

avg_SPUE_vs_num_stormy_days <- lm(mean_whales_per_day ~ num_stormy_days_all_CVU, data = whale_num_storm_all_summary)
summary(avg_SPUE_vs_num_stormy_days)

cor.test(~num_stormy_days_all_CVU + mean_whales_per_day, data = whale_num_storm_all_summary)
# cor.test(~mean_whales_per_day +num_stormy_days_all_CVU, data = whale_num_storm_all_summary)



ggplot(data = whale_num_storm_strong_summary, aes(x = num_stormy_days_strong_CVU, y = mean_whales_per_day, color = as.character(Year))) + 
  geom_point(size = 3) +
  labs(title = "Number of stormy days vs average whale SPUE in each year",
       x = "Number of days with a storm per year", y = "Average daily whale SPUE per year", color = "Year")
ggsave(here::here("Figures", "strong_stormy_daysvs_avg_SPUE_per_year.png"), width = 7.1, height = 4.2)  

avg_SPUE_vs_num_strong_stormy_days <- lm(mean_whales_per_day ~ num_stormy_days_strong_CVU, data = whale_num_storm_strong_summary)
summary(avg_SPUE_vs_num_strong_stormy_days)

# labs(title = "number of strong stormy (cvu >3) days per year vs avg whale SPUE", y = "number of strong stormy days per year")
cor.test(~num_stormy_days_strong_CVU + mean_whales_per_day, data = whale_num_storm_strong_summary)


##----SPUE vs avg CVU intensity----
#during the field season & removed days where CVU < 1
whale_strong_storm_summary <- whale_storm_dummy %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>%
  dplyr::filter(Avg_CVU_per_day != 0.99) %>% 
  dplyr::summarize(mean_whales = mean(Whales_per_day_effort_correct), avg_strong_CVU = mean(Avg_CVU_per_day))

ggplot(data = whale_strong_storm_summary, aes(x = mean_whales, y = avg_strong_CVU, color = as.character(Year))) + 
  geom_point(size = 3) +
  labs(title = "avg daily whale vs avg strong 1")

cor.test(~mean_whales + avg_strong_CVU, data = whale_strong_storm_summary)

#Only removed days where CVU < 1
whale_strong_storm_summary2 <- whale_storm_dummy %>%
  dplyr::group_by(Year) %>%
  dplyr::filter(Avg_CVU_per_day != 0.99) %>% 
  dplyr::summarize(mean_whales = mean(Whales_per_day_effort_correct), avg_strong_CVU = mean(Avg_CVU_per_day))

ggplot(data = whale_strong_storm_summary2, aes(x = mean_whales, y = avg_strong_CVU, color = as.character(Year))) + 
  geom_point(size = 3) +
  labs(title = "avg daily whale vs avg strong 2")
cor.test(~mean_whales + avg_strong_CVU, data = whale_strong_storm_summary2)

#Days during field season, included changed 0.99 CVU to 0 CVU then calculated average CVU for field season
whale_strong_storm_summary3 <- whale_storm_dummy %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>%
  dplyr::mutate(Avg_CVU_per_day_alt = ifelse(Avg_CVU_per_day == 0.99, 0, Avg_CVU_per_day)) %>% 
  dplyr::summarize(mean_whales = mean(Whales_per_day_effort_correct), avg_strong_CVU = mean(Avg_CVU_per_day_alt))

ggplot(data = whale_strong_storm_summary3, aes(x = mean_whales, y = avg_strong_CVU, color = as.character(Year))) + 
  geom_point(size = 3) +
  labs(title = "avg daily whale vs avg strong cvu 3")
cor.test(~mean_whales + avg_strong_CVU, data = whale_strong_storm_summary3)


#Test 4, split up 
whale_strong_storm_summary_4_whale_half <- whale_storm_dummy %>% 
  select(-Avg_CVU_per_day) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>% 
  dplyr::summarize(mean_whales_per_day = mean(Whales_per_day_effort_correct))
whale_strong_storm_summary_4_storm_half <- whale_storm_dummy %>% 
  select(-Whales_per_day_effort_correct) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::filter(During_FieldSeason != "No") %>%
  dplyr::filter(Avg_CVU_per_day != 0.99) %>% 
  dplyr::summarize(avg_strong_CVU = mean(Avg_CVU_per_day))

whale_strong_storm_summary4 <- full_join(whale_strong_storm_summary_4_whale_half, whale_strong_storm_summary_4_storm_half, by = "Year")
ggplot(data = whale_strong_storm_summary4, aes(x = mean_whales_per_day, y = avg_strong_CVU, color = as.character(Year))) + 
  geom_point(size = 3) +
  labs(title = "avg daily whale vs avg strong cvu 4")
cor.test(~mean_whales_per_day + avg_strong_CVU, data = whale_strong_storm_summary4)


#----Initial Geolocation Mapping----
#whale map
whale_location_range <- c(-70, -67.5, -60, -62.5)     #Left, Bot, Right, Top

whale_map_base <- get_stadiamap(bbox = whale_location_range, zoom = 7, 
                          crop = F, color = "color")

ggmap(whale_map_base) + 
  geom_point(data = whale_sight_clean,
             aes(x = Longitude, y = Latitude), alpha = 0.5, color = "red") 

#Map showing whale data coverage within Stack's PAL LTER grid
stack_map_range <- c(-80, -70, -61, -63)

stack_map_base <- get_stadiamap(bbox = stack_map_range, zoom = 3, crop = T)

ggmap(stack_map_base)

ggmap(stack_map_base) + 
  geom_point(data = storm_clean_PAL_LTER_allCVU, #storm_select_test,
             aes(x = Longitude, y = Latitude, size = Intensity_CVU), alpha = 0.25) + 
  geom_point(data = whale_sight_clean,
             aes(x = Longitude, y = Latitude, color = Species), alpha = 0.5)+#, color = "red")  + 
  labs(color = "Whale Observations")

ggmap(stack_map_base) + 
  geom_point(data = storm_clean_PAL_LTER_morethan3CVU, #storm_select_test,
             aes(x = Longitude, y = Latitude, size = Intensity_CVU), alpha = 0.25) + 
  geom_point(data = whale_sight_clean,
             aes(x = Longitude, y = Latitude, color = Species), alpha = 0.5)+#, color = "red")  + 
  labs(color = "Whale Observations")


#storm location explore plot
ggplot(data = storm_clean, aes(x = Longitude, y = Latitude)) +
  geom_point()


#----Data Analysis & Stats Analysis----
##----Time Lag----      
#can also use storm_20xx_ts and whale_20xx_ts below
ccf_2015 <- ccf(x = storm_distinct2015, y = whale_distinct2015, type = "correlation")
ccf_2015      #sig @ time lag = none (not significant)
png("Figures/Time-Lag/ccf_2015.png", width = 857, height = 534)
plot(ccf_2015, main = "Average daily storm CVU vs total Humpback whale SPUE in 2015")
dev.off()

ccf_2016 <- ccf(x = storm_distinct2016, y = whale_distinct2016, type = "correlation")
ccf_2016      #sig @ time lag = -10, -11, -12, -13
png("Figures/Time-Lag/ccf_2016.png", width = 857, height = 534)
plot(ccf_2016, main = "Average daily storm CVU vs total Humpback whale SPUE in 2016")
dev.off()

ccf_2017 <- ccf(x = storm_distinct2017, y = whale_distinct2017, type = "correlation")
ccf_2017      #sig @ time lag = -12
png("Figures/Time-Lag/ccf_2017.png", width = 857, height = 534)
plot(ccf_2017, main = "Average daily storm CVU vs total Humpback whale SPUE in 2017")
dev.off()

ccf_2018 <- ccf(x = storm_distinct2018, y = whale_distinct2018, type = "correlation")
ccf_2018      #sig @ time lag = -4
png("Figures/Time-Lag/ccf_2018.png", width = 857, height = 534)
plot(ccf_2018, main = "Average daily storm CVU vs total Humpback whale SPUE in 2018")
dev.off()

ccf_2019 <- ccf(x = storm_distinct2019, y = whale_distinct2019, type = "correlation")
ccf_2019      #sig @ time lag = none (not significant)
png("Figures/Time-Lag/ccf_2019.png", width = 857, height = 534)
plot(ccf_2019, main = "Average daily storm CVU vs total Humpback whale SPUE in 2019")
dev.off()

# ccf_2019_alt <- ccf(x = storm_distinct2019, y = whale_distinct2019_MAG_edit, type = "correlation")
# ccf_2019_alt  #sigh @ time lag = none (not signficant)

ccf_2020 <- ccf(x = storm_distinct2020, y = whale_distinct2020, type = "correlation")
ccf_2020      #sig @ time lag = +10
png("Figures/Time-Lag/ccf_2020.png", width = 857, height = 534)
plot(ccf_2020, main = "Average daily storm CVU vs total Humpback whale SPUE in 2020")
dev.off()


#2016
lag2.plot(storm_distinct2016, whale_distinct2016, 15)

ts(storm_2016_ts)

alldata_2016 = ts.intersect(storm_2016_ts,
                            whale_2016_ts,
                            storm_2016_lagn10 = stats::lag(storm_2016_ts, -10),
                            storm_2016_lagn11 = stats::lag(storm_2016_ts, -11),
                            storm_2016_lagn12 = stats::lag(storm_2016_ts, -12),
                            storm_2016_lagn13 = stats::lag(storm_2016_ts, -13))
# 
# whale_storm_lm2016 = lm(whale_2016_ts ~ storm_2016_lagn10 + storm_2016_lagn11 +
#                         storm_2016_lagn12 + storm_2016_lagn13,
#                     data = alldata_2016)
# summary(whale_storm_lm2016)
# 
# whale_storm_lm2016_b = lm(whale_2016_ts ~ storm_2016_lagn10 +
#                           storm_2016_lagn11 + storm_2016_lagn13,
#                         data = alldata_2016)
# summary(whale_storm_lm2016_b)
# 
# whale_storm_lm2016_c = lm(whale_2016_ts ~ storm_2016_lagn10 +
#                             storm_2016_lagn11,
#                           data = alldata_2016)
# summary(whale_storm_lm2016_c)

whale_storm_lm2016_d = lm(whale_2016_ts ~ storm_2016_lagn10, data = alldata_2016)
summary(whale_storm_lm2016_d)

# whale_storm_lm2016_e = lm(whale_2016_ts ~ storm_2016_lagn11, data = alldata_2016)
# summary(whale_storm_lm2016_e)
# 
# whale_storm_lm2016_f = lm(whale_2016_ts ~ storm_2016_lagn13, data = alldata_2016)
# summary(whale_storm_lm2016_f)



##0-inflated Poisson transformation test
zinf2016 <- zeroinfl(as.numeric(whale_2016_ts) ~ storm_2016_lagn10, data = alldata_2016)



ggplot(data = as.data.frame(alldata_2016),
       aes(x = storm_2016_lagn10, y = whale_2016_ts)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "2016 Linear regression model with a time lag of -10 days",
       x = "daily average storm CVU values", y = "daily average whale SPUE values")
ggsave(here::here("Figures", "2016_storm_and_whale_ts_lagn10.png"), width = 7.1, height = 4.2)



#2017
all_data_2017 = ts.intersect(storm_2017_ts,
                             whale_2017_ts,
                             storm_2017_lagn12 = stats::lag(storm_2017_ts, -12))

whale_storm_lm2017 = lm(whale_2017_ts ~ storm_2017_lagn12, data = all_data_2017)
summary(whale_storm_lm2017)

# whale_storm_lm2017_logx = lm(whale_2017_ts ~ log10(storm_2017_lagn12), data = all_data_2017)
# summary(whale_storm_lm2017_logx)

ggplot(data = as.data.frame(all_data_2017),
       aes(x = storm_2017_lagn12, y = whale_2017_ts)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  labs(title = "2017 Linear regression model with a time lag of -12 days",
       x = "daily average storm CVU values", y = "daily average whale SPUE values")

ggsave(here::here("Figures", "2017_storm_and_whale_ts_lagn12.png"), width = 7.1, height = 4.2)



#2018
all_data_2018 = ts.intersect(storm_2018_ts,
                             whale_2018_ts,
                             storm_2018_lagn4 = stats::lag(storm_2018_ts, -4))

whale_storm_lm2018 = lm(whale_2018_ts ~ storm_2018_lagn4, data = all_data_2018)
summary(whale_storm_lm2018)

ggplot(data = as.data.frame(all_data_2018),
       aes(x = storm_2018_lagn4, y = whale_2018_ts)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  labs(title = "2018 Linear regression model with a time lag of -4 days",
       x = "daily average storm CVU values", y = "daily average whale SPUE values")

ggsave(here::here("Figures", "2018_storm_and_whale_ts_lagn4.png"), width = 7.1, height = 4.2)

#2020
all_data_2020 = ts.intersect(storm_2020_ts,
                             whale_2020_ts,
                             storm_2020_lag10 = stats::lag(storm_2020_ts, 10))
whale_storm_lm2020 = lm(whale_2020_ts ~ storm_2020_lag10, data = all_data_2020)
summary(whale_storm_lm2020)

# #GLSR
# ws_2015_lm <- lm(Whales_per_day_effort_correct ~ Avg_CVU_per_day, data = whale_storm_distinct2015)
# 
# acf(ws_2015_lm$residuals)
# pacf(ws_2015_lm$residuals)
# 
# 
# ws_2018_lm <- lm(Whales_per_day_effort_correct ~ Avg_CVU_per_day, data = whale_storm_distinct2018)
# 
# acf(ws_2018_lm$residuals)
# pacf(ws_2018_lm$residuals) #AR(p=2) model
# 
# ws_2018_ar2 <- gls(Whales_per_day_effort_correct ~ Avg_CVU_per_day, data = whale_storm_distinct2018, 
#                    correlation = corARMA(p=2))
# summary(ws_2018_ar2)

#----Graphs and Visual Analysis----
##----Year-Year----
##avg cvu per day (all CVU)
ggplot(data = storm_all_distinct,
       aes(x = Julian_new, y = Avg_CVU_per_day)) + 
  geom_line(color= "red") + 
  geom_point(color = "red", data = storm_real_distinct) +
  facet_wrap(~Year) + 
  ylim(0, 9) + 
  labs(title = "average CVU per day, all CVU data pts",
       x = "day of the year")

##avg CVU per day (strong CVU (>3 CVU))
# ggplot(data = storm_clean_PAL_LTER_morethan3CVU,
#        aes(x = Julian_new, y = Avg_strong_CVU_per_day)) + 
#   geom_line(color= "red") + 
#   geom_point(color = "red") +
#   facet_wrap(~Year) +
#   ylim(0, 9) + 
#   labs(title = "average strong CVU per day, CVU data pts > 3 CVU",
#        x = "day of the year")

##number whale sightings per julian day 
ggplot(data = whale_sight_clean,
       aes(x = Julian_new, y = Whales_per_day)) + 
  geom_line(color= "blue") + #aes(color = Year_char))  +
  geom_point(color = "blue") + 
  facet_wrap(~ Year) + 
  labs(title = "number of whales per day of the year",
       x = "day of the year")
  
##number of effort corrected whale sighting per julian day
ggplot(data = whale_sight_clean,
       aes(x = Julian_new, y = Whales_per_day_effort_correct)) + 
  geom_line(color= "blue") + #aes(color = Year_char))  +
  geom_point(color = "blue") + 
  facet_wrap(~ Year) + 
  labs(title = "number of whales, effort corrected, per day of the year",
       x = "day of the year")

##effort corrected whale &&& avg CVU per day
# ggplot(data = whale_sight_clean, 
#   aes(x = Julian_new, y = Whales_per_day_effort_correct)) + 
#   geom_line(color = "blue") +
#   geom_point(color = "blue") + 
#   geom_line(data = storm_clean_PAL_LTER_morethan3CVU, aes(y = Avg_strong_CVU_per_day), color = "red") +
#   geom_point(data = storm_clean_PAL_LTER_morethan3CVU, aes(y = Avg_strong_CVU_per_day), color = "red") +
#   facet_wrap(~ Year) +
#   scale_y_log10() + 
#   labs(x = "Day of the Year", y = "Scale", title = "Effort corrected Whales per day (blue) and average CVU >3 per day (red)")

  
ggplot(data = whale_sight_clean, 
       aes(x = Julian_new, y = Whales_per_day_effort_correct)) + 
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  geom_line(data = storm_clean_PAL_LTER_allCVU, aes(y = Avg_CVU_per_day), color = "red") +
  geom_point(data = storm_real_distinct, aes(y = Avg_CVU_per_day), color = "red") +
  facet_wrap(~ Year) +
  scale_y_log10() + 
  labs(x = "Day of the Year", y = "Scale", title = "Effort corrected Whales per day (blue) and average CVU, all observed, per day (red)")



##----Month-Month----
##number whale per Month
ggplot(data=whale_sight_clean,
       aes(x = Month, y = Group.Size)) + 
  geom_col(fill= "blue") +
  facet_wrap(~Year) +
  labs(y = "Number of Whales per month") 

##number whale (effort corrected) per month                     
ggplot(data=whale_sight_distinct,
       aes(x = Month, y = mean(Whales_per_day_effort_correct))) +
  geom_col(fill= "blue") +
  labs(y = "Mean Number of Whales per month, effort corrected") +
  facet_wrap(~ Year)
  # scale_y_log10()
  
## x = CVU value, y = freq above >1
ggplot(data = storm_real_distinct,
       aes(x = Avg_CVU_per_day)) +
  geom_histogram()

ggplot(data = storm_all_distinct,
       aes(x = Avg_CVU_per_day)) +
  geom_histogram()

ggplot(data = storm_event_distinct, 
       aes(x = Length_day_storm)) + 
  geom_histogram(binwidth =1) 

ggplot(data = storm_event_distinct, 
       aes(x = Days_since_last_storm)) + 
  geom_histogram(binwidth = 1) 

ggplot(data = storm_event_distinct, 
       aes(x = Days_since_last_storm)) + 
  geom_boxplot() 

##----Correlation plots----
ggplot(data = whale_storm_allCVU, 
       aes(x = Avg_CVU_per_day, y = Whales_per_day_effort_correct)) +
  geom_jitter() +
  theme_classic() +
  geom_smooth(method = "lm")

whale_storm_allCVU_no_na <- whale_storm_allCVU %>% 
  drop_na()

cor.test(~Avg_CVU_per_day + Whales_per_day_effort_correct, data = whale_storm_allCVU_no_na,
         method = "spearman")

# cor.test(~Avg_CVU_per_day + Whales_per_day_effort_correct, data = whale_storm_allCVU_no_na)

##----Julian CPUE bins----
whale_storm_bin <- whale_storm_dummy %>% 
  dplyr::mutate(Julian_bin = cut(Julian_new, breaks = c(0, 7, 14, 21, 28, 35, 42, 49, 
                                                 56, 63, 70, 77, 84, 91, 98, 105, 
                                                 112, 119, 126))) %>% 
  dplyr::group_by(Year, Julian_bin) %>%
  dplyr::mutate(wk_whale_avg = mean(Whales_per_day_effort_correct)) %>% 
  dplyr::mutate(Julian_group = as.character(Julian_bin)) 
  

whale_storm_bin$Julian_group <- gsub('\\(0,7\\]', '1',
                                gsub('\\(7,14\\]', '2',
                                gsub('\\(14,21\\]', '3',
                                gsub('\\(21,28\\]', '4',
                                gsub('\\(28,35\\]', '5',
                                gsub('\\(35,42\\]', '6',
                                gsub('\\(42,49\\]', '7',
                                gsub('\\(49,56\\]', '8',
                                gsub('\\(56,63\\]', '9',
                                gsub('\\(63,70\\]', '10',
                                gsub('\\(70,77\\]', '11',
                                gsub('\\(77,84\\]', '12',
                                gsub('\\(84,91\\]', '13',
                                gsub('\\(91,98\\]', '14',
                                gsub('\\(98,105\\]', '15',
                                gsub('\\(105,112\\]', '16',
                                gsub('\\(112,119\\]', '17',
                                gsub('\\(119,126\\]', '18', whale_storm_bin$Julian_group)))))))))))))))))) %>% 
  as.numeric()

ggplot(data = whale_storm_bin, 
       aes(x = Julian_bin, y = wk_whale_avg)) + #, color = as.character(Year))) + 
  geom_point() + 
  facet_wrap(~Year) +
  # theme_minimal() + 
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, hjust = 1))

whale_weekly_summary <- summarySE(whale_storm_bin, measurevar = "Whales_per_day_effort_correct", groupvars = c("Year", "Julian_group")) %>% 
  dplyr::mutate(avg_whales_julian_week_effort_correct = Whales_per_day_effort_correct) %>% 
  select(-Whales_per_day_effort_correct) %>% 
  dplyr::filter(avg_whales_julian_week_effort_correct != 0)

ggplot(data = whale_weekly_summary, 
       aes(x = Julian_group, y = avg_whales_julian_week_effort_correct, 
           ymin = avg_whales_julian_week_effort_correct - se, ymax = avg_whales_julian_week_effort_correct + se)) + #, color = as.character(Year))) + 
  geom_pointrange() + 
  facet_wrap(~Year) +
  theme_bw() +
  scale_x_continuous(breaks= scales::pretty_breaks(n=16)) + 
  labs(title = "Average Humpback whale SPUE per week of the year", 
       x = "Week of the year", y = "average whale SPUE")
ggsave(here::here("Figures", "whale_SPUE_per_week.png"), width = 7.1, height = 4.2)

ggplot(data = whale_storm_bin,
       aes(x = Julian_new, y = Whales_per_day_effort_correct)) + 
  geom_point() +
  facet_wrap(~Year)

###----Penguin Team Data----
ggplot(data = pen_whale_clean, 
       aes(x = Julian_bin, y = Week_whale_avg)) + #, color = as.character(Year))) + 
  geom_point() + 
  facet_wrap(~Year) +
  theme_minimal() + 
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0, 3)

ggplot(data = pen_whale_clean, 
       aes(x = Julian_bin, y = Transect_Whale_week_avg, color = Transect)) + #, color = as.character(Year))) + 
  geom_point() + 
  facet_wrap(~Year) +
  theme_minimal() + 
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0, 4)


#----Trash Bin----
##number whale (effort correct) per month + 
##whale over time, julian day
# whale_sight_clean %>% 
#   dplyr::filter(Year == 2015) %>% 
# ggplot(aes(x = Julian_new, y = Whales_per_day)) + 
#   geom_line() + 
#   labs(title = "2015")
# 
# whale_sight_clean %>% 
#   dplyr::filter(Year == 2016) %>% 
# ggplot(aes(x = Julian_new, y = Whales_per_day)) + 
#   geom_line() +
#   labs(title = "2016")
# 
# whale_sight_clean %>% 
#   dplyr::filter(Year == 2017) %>% 
# ggplot(aes(x = Julian_new, y = Whales_per_day)) + 
#   geom_line() +
#   labs(title = "2017")
# 
# whale_sight_clean %>% 
#   dplyr::filter(Year == 2018) %>% 
# ggplot(aes(x = Julian_new, y = Whales_per_day)) + 
#   geom_line() +
#   labs(title = "2018")
# 
# whale_sight_clean %>% 
#   dplyr::filter(Year == 2019) %>% 
# ggplot(aes(x = Julian_new, y = Whales_per_day)) + 
#   geom_line() +
#   labs(title = "2019")
# 
# whale_sight_clean %>% 
#   dplyr::filter(Year == 2020) %>% 
# ggplot(aes(x = Julian_new, y = Whales_per_day)) + 
#   geom_line() +
#   labs(title = "2020")
# 
