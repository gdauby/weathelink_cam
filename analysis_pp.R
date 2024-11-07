
library(tidyverse)
library(dplyr)
library("imputeTS")
library("climatol") # https://climatol.eu/
source("functions_pp.R")

######
###### Read & reformat data
######

path_to_data = "ENEF01_1-10-23_00-00_1_Year_1727701463_v2.csv"
#path_to_data = "Bouamir_Research_Station_11-19-23_23-00_1_Year_1720206107_v2.csv"

dataset <- read_csv(path_to_data,
                    skip = 5,
                    col_types = cols(.default = "c"),
                    locale = locale(encoding = "latin1")
                    )

all_colnames <- tibble(original_colnam = do.call(rbind, strsplit(names(dataset), " - "))[,1])

all_colnames <- 
  all_colnames %>% 
  mutate(modified_colnam = str_to_lower(original_colnam)) %>%
  mutate(modified_colnam = str_replace_all(modified_colnam, pattern = "&", replacement = "")) %>% 
  mutate(modified_colnam = str_replace_all(modified_colnam, pattern = "  ", replacement = " ")) %>% 
  mutate(modified_colnam = str_replace_all(modified_colnam, pattern = " ", replacement = "_"))

names(dataset) <- 
  all_colnames$modified_colnam

dataset %>% 
  mutate(inside_temp = str_replace(inside_temp, ",", ".")) %>% 
  mutate(inside_temp = as.numeric(inside_temp))

dataset_num <- 
  dataset %>% 
  mutate(across(2:ncol(dataset), ~ na_if(., "--"))) %>%
  mutate(across(everything(), ~ str_replace(., ",", "."))) %>% 
  mutate(across(!contains("wind_dir") & !contains("date_"), ~ as.numeric(.)))

tmp = strsplit(dataset_num$date_time, "/", fixed = TRUE)
formatDate =  ifelse(max(as.numeric(sapply(tmp, "[", 1))) == 12,
                     "%m/%d/%y %H:%M",
                     "%d/%m/%y %H:%M")

dataset_num <- 
  dataset_num %>% 
  mutate(date = as.POSIXct(date_time, format = formatDate)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         week = week(date),
         hour = hour(date),
         minute = minute(date)) %>% 
  relocate(year, .after = date_time) %>% 
  relocate(month, .after = date_time) %>% 
  relocate(day, .after = date_time) %>% 
  relocate(hour, .after = date_time) %>% 
  relocate(week, .after = date_time) %>% 
  relocate(minute, .after = date_time)

dataset_num$date_julian <-  julian(dataset_num$date, 
                                   origin = min(dataset_num$date)
                                   )

head(dataset_num[, c("date_time", "date", "date_julian")])

######
###### Explore missing data
######

### 1. Check missing data with respect to "15 min" (or "30 min", or "1 hour") theoretical records
check_NA_distrib(data = dataset_num, var_name = "rain", tempRes = "15 min")
check_NA_distrib(data = dataset_num, var_name = "temp", tempRes = "15 min")
check_NA_distrib(data = dataset_num, var_name = "solar_rad", tempRes = "15 min")

### 2. Check missing data in the table itself
ggplot_na_distribution(x = dataset_num$temp,
                       x_axis_labels = dataset_num$date)


ggplot_na_distribution(x = dataset_num$rain,
                       x_axis_labels = dataset_num$date)


ggplot_na_distribution(x = dataset_num$solar_rad,
                       x_axis_labels = dataset_num$date)

### 3. Check missing data per day in % of a theoretical temporal resolution
col_name = "temp"
tempRes = "30 min"  # temporal resolution of theoretical TS
data = regularize_TS.df(data = dataset_num, tempRes = tempRes)
Daily_interval = (24*60)/as.numeric(strsplit(tempRes, " ")[[1]][1])
ggplot_na_distribution2(x = data[which(names(data) == col_name)],
                        interval_size = Daily_interval)

### 4. Filter out some days with too many NAs
# Compute table of days & associated proportion of NA (NA in the sense : there is no row in the table)
tmp = data.table(monitoredDays = as.integer(dataset_num$date_julian),
                 TS_lags = as.numeric(dataset_num$date_julian - dplyr::lag(dataset_num$date_julian))
                 )
tmp.fullNA = tmp[, by=c("monitoredDays"), .(PropOfNA=sum(TS_lags, na.rm=T))]

# List days with less than 90 % of data - to be removed
julianDaysNA = which(tmp.fullNA$PropOfNA < .9 | tmp.fullNA$PropOfNA > 1)
julianDaysNA
toBeRemoved.df = dataset_num[which(as.integer(dataset_num$date_julian) %in% julianDaysNA),]  # data to be removed because the records are missing on ALL rows
head(toBeRemoved.df)

######
###### Work on ONE wheather variable
######

colName = "temp"

### 1. Remove NA data specific to the focal variable
tmp$TS_lags[which(is.na(dataset_num[,which(names(dataset_num) == colName)]) == T)] = NA
tmp.varNA = tmp[, by=c("monitoredDays"), .(PropOfNA=sum(TS_lags, na.rm=T))]

julianDaysNA = which(tmp.varNA$PropOfNA < .9 | tmp.varNA$PropOfNA > 1)
julianDaysNA # julian days to be removed

dataset_num$date_julian_Day = as.integer(dataset_num$date_julian)

data = dataset_num[which(dataset_num$date_julian_Day %!in% julianDaysNA),]  # temporary 'NA-free' dataset (given conditions on tmp.varNA$PropOfNA above)

data.dt = as.data.table(data)



data.dt = data.dt[, by=c("month", "date_julian_Day"), .("Precip" = sum(rain, na.rm = T),       # total daily precipitation
                                                        "MaxT" = max(temp, na.rm = T),         # maximum daily temperature
                                                        "MinT" = min(temp, na.rm = T),         # minimum daily temperature
                                                        "AbsMinT" = min(abs(temp), na.rm = T)  # Absolute minimum daily temperature
                                                       )]

data.dt = data.dt[, by=c("month"), .("Precip" = sum(Precip, na.rm = T),        # total monthly precipitation
                                     "MaxT" = mean(MaxT, na.rm = T),           # mean maximum daily temperature
                                     "MinT" = mean(MinT, na.rm = T),           # mean minimum daily temperature
                                     "AbsMinT" = mean(abs(AbsMinT), na.rm = T) # mean Absolute minimum daily temperature
                                     )]


data.dt = data.dt[order(data.dt$month),]
tmpx = as.data.frame(t(data.dt[,-1]))
data(climatol_data)
names(tmpx) = names(datcli)
diagwl(tmpx, est="Manchester [Lat: 53.5, Lon: -2.2]", alt="38", per="1970 to 2000")
diagwl(tmpx,cols=NULL, est="My Airport", alt=100, per="2023-2024", mlab="en")








