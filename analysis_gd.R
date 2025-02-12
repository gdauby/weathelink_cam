

library(tidyverse)
source("function_julian_date.R")

dataset <- read_csv(
  "ENEF01_1-10-23_00-00_1_Year_1727701463_v2.csv",
  skip = 5,
  col_types = cols(.default = "c"),
  locale = locale(encoding = "latin1")
)

all_colnames <- tibble(original_colnam = names(dataset))

all_colnames <- 
  all_colnames %>% 
  mutate(modified_colnam = str_to_lower(original_colnam)) %>%
  mutate(modified_colnam = str_replace_all(modified_colnam, pattern = "&", replacement = "")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- °c")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- %")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- hpa")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- m/s")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- mm")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- m")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- w/m^2")) %>% 
  mutate(modified_colnam = str_remove_all(string = modified_colnam, pattern = "- ly")) %>% 
  mutate(modified_colnam = str_squish(modified_colnam)) %>%
  mutate(modified_colnam = str_replace_all(modified_colnam, pattern = " ", replacement = "_"))

names(dataset) <- 
  all_colnames$modified_colnam

# dataset %>% slice(1:10) %>% View

# dataset %>% 
#   mutate(inside_temp = str_replace(inside_temp, ",", ".")) %>% 
#   mutate(inside_temp = as.numeric(inside_temp))

dataset_num <- 
  dataset %>% 
  mutate(across(2:ncol(dataset), ~ na_if(., "--"))) %>%
  mutate(across(everything(), ~ str_replace(., ",", "."))) %>% 
  mutate(across(!contains("wind_dir") & !contains("date_"), ~ as.numeric(.)))

# dataset_num %>% rowwise() %>%
#   filter(!all(is.na(c_across(where(is.numeric)))))

tmp = strsplit(dataset_num$date_time, "/", fixed = TRUE)
formatDate =  ifelse(max(as.numeric(sapply(tmp, "[", 1))) == 12,
                     "%m/%d/%Y %H:%M",
                     "%d/%m/%Y %H:%M")

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

dataset_num <- 
  dataset_num %>% 
  mutate(date_julian = sapply(dataset_num$date, calculate_julian_day))

## Explore missing data #####

## sequence of days from first to last day of measurements
day_seq <-
  tibble(day_int = seq(
    from = min(dataset_num$date),
    to = max(dataset_num$date),
    by = "1 day"
  ))

hour(day_seq$day_int) <- 12

day_seq <- 
  day_seq %>% 
  mutate(month = month(day_int),
         year = year(day_int),
         day = day(day_int))

day_seq <- 
  day_seq %>% 
  mutate(date_julian = sapply(day_seq$day_int, calculate_julian_day)) %>% 
  select(date_julian, day, month, year) %>% 
  distinct()

### table with number of NA and number for TEMPERATURE
missing_val_per_day <- 
  dataset_num %>% 
  group_by(date_julian, day, month, year) %>% 
  mutate(time_int = abs(date[2] - date[1])) %>% 
  summarise(nbe_na = sum(is.na(temp)),
            nbe_val = n(),
            time_int = min(time_int)) %>% 
  mutate(prop_missing_val = round(nbe_na/nbe_val*100, 1))

## merging with sequence of days to detect days with no values
missing_val_per_day <- 
  day_seq %>% 
  left_join(missing_val_per_day)

### table with time interval for each day and the maximam number of values it means
interval_measures <- 
  missing_val_per_day %>% 
  group_by(time_int) %>% 
  summarise(nbe = n(),
            max_val = max(nbe_val)) %>% 
  ungroup() %>% 
  mutate(theoretical_max_val = 24/(as.numeric(time_int)/3600))

### adding the time interval for each day and compute the proportion of values indeed measured for each day
missing_val_per_day <- 
  missing_val_per_day %>% 
  left_join(interval_measures %>% 
              select(theoretical_max_val, max_val, time_int)) %>% 
  mutate(prop_val_measured = round(nbe_val / theoretical_max_val*100, 1))


## list of days with no values
missing_val_per_day %>% 
  filter(is.na(nbe_val))

### list of days for which we have less than 80 % of values
missing_val_per_day %>% 
  filter(prop_val_measured < 80)

### 50%
missing_val_per_day %>% 
  filter(prop_val_measured < 50)


days_to_remove <- missing_val_per_day %>% 
  filter(prop_val_measured < 80 | is.na(nbe_val))

dataset_filtered <- 
  dataset_num %>% 
  filter(!date_julian %in% days_to_remove$date_julian)


rain_month <- 
  dataset_filtered %>% 
  group_by(month, year) %>% 
  summarise(rain = sum(rain, na.rm = T)) %>% 
  mutate(month_txt = paste(year, month, "01", sep = "-")) %>% 
  mutate(month_d = as.Date(month_txt)) %>%
  mutate(month_d = format(month_d, "%Y-%m")) %>% 
  ungroup() %>% 
  left_join(tibble(DOY = seq(15, 365, by = 365/12),
                   month = seq(1, 12, 1))
  )


temp_month <- 
  dataset_filtered %>% 
  group_by(date_julian, month, year) %>% 
  summarise(temp_mean = mean(temp, na.rm = T),
            temp_min = min(low_temp, na.rm = T),
            temp_max = max(high_temp, na.rm = T),
            et_mean = mean(et, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month, year) %>% 
  summarise(temp_mean = mean(temp_mean, na.rm = T),
            temp_min = mean(temp_min, na.rm = T),
            temp_max = mean(temp_max, na.rm = T),
            et_mean = mean(et_mean, na.rm = T)) %>% 
  mutate(month_txt = paste(year, month, "01", sep = "-")) %>% 
  mutate(month_d = as.Date(month_txt)) %>% 
  mutate(month_d = format(month_d, "%Y-%m")) %>% 
  ungroup()



monthly_data <- 
  rain_month %>%
  left_join(temp_month %>% select(month_d, starts_with("temp"), et_mean)) %>% 
  mutate(Mth = factor(month.abb[month], 
                      levels = c("Jan", "Feb", "Mar", "Apr",
                                 "May", "Jun", "Jul", "Aug",
                                 "Sep", "Oct", "Nov", "Dec")))


## rescale temperature
rain_scale_up <- 300
rain_scale_down <- 20
temp_scale_up <- 32
temp_scale_down <- 18

m_ <- (rain_scale_up - rain_scale_down) / (temp_scale_up - temp_scale_down)
q_ <- rain_scale_up - m_*temp_scale_up

monthly_data <- 
  monthly_data %>% 
  mutate(temp_scaled_mean = q_ + m_ * temp_mean,
         temp_scaled_min = q_ + m_ * temp_min,
         temp_scaled_max = q_ + m_ * temp_max)

ggplot() +
  geom_bar(monthly_data, mapping = aes(x = DOY, y = rain), fill = "grey",
           stat = "identity", width = 28) +
  geom_line(monthly_data, mapping = aes(x = DOY, y = temp_scaled_mean), col = "black") +
  geom_line(monthly_data, mapping = aes(x = DOY, y = temp_scaled_min), col = "blue") +
  geom_line(monthly_data, mapping = aes(x = DOY, y = temp_scaled_max), col = "red") +
  scale_x_continuous(breaks = c(365/12, 365/12*4, 365/12*8, 365) - 15, 
                     labels = c("Jan", "Apr", "Aug", "Dec"),
                     name = "") +
  scale_y_continuous(name = "Rain (mm)", breaks = seq(0, 600, 50),
                     sec.axis = sec_axis(~ (. - q_)/m_, 
                                         name = "Daily Temperature (°C)",
                                         breaks = c(15, 20, 25, 30, 35))) +
  facet_wrap(~year) +
  theme_bw()


### Extreme rainfall events
dataset_filtered %>% 
  group_by(date_julian, day, month, year) %>% 
  summarise(rain = sum(rain)) %>% 
  filter(rain > 100)


### rain events identified as consecutives hours where rain > 0
rain_events <- 
  dataset_filtered %>% 
  select(date_julian, hour, rain, avg_wind_speed, high_wind_speed, day, month, year, date) %>% 
  group_by(date_julian, hour, day, month, year, date) %>% ### grouping by hour
  summarise(rain = sum(rain, na.rm = T), ### hourly rain
            avg_avg_wind_speed = mean(avg_wind_speed, na.rm = T), ## averaged averaged wind speed during event
            max_avg_wind_speed = max(avg_wind_speed, na.rm = T), ### maximal averaged wind speed during event
            avg_high_wind_speed = mean(high_wind_speed, na.rm = T), ### averaged maximal wind speed during event
            max_high_wind_speed = max(high_wind_speed, na.rm = T)) %>%  ### maximal maximal wind speed during event
  ungroup() %>% 
  mutate(rain_event = cumsum(rain == 0)) %>% ### attributing group
  filter(rain > 0) ### filtering out when no rain
 
rain_events_grouped <- 
  rain_events %>% 
  group_by(rain_event) %>% 
  summarise(rain = sum(rain, na.rm = T),
            avg_avg_wind_speed = mean(avg_avg_wind_speed, na.rm = T),
            max_avg_wind_speed = max(max_avg_wind_speed, na.rm = T),
            max_high_wind_speed = max(max_high_wind_speed, na.rm = T),
            avg_high_wind_speed = mean(avg_high_wind_speed, na.rm = T),
            date_julian = paste(as.character(unique(date_julian)), collapse ="-")
            ) %>% 
  mutate(across(contains("wind_speed"), ~units::set_units(., m/s))) %>% 
  mutate(across(contains("wind_speed"), ~units::set_units(., km/h)))


extreme_events <- 
  rain_events_grouped %>% 
  filter(max_avg_wind_speed > units::set_units(10, km/h) | 
         rain > 50)

rain_events %>% 
  filter(rain_event %in% extreme_events$rain_event) %>% 
  group_by(month, year) %>% 
  summarise(nbe_events = length(unique(rain_event)))


rain_events_month <- 
  rain_events_grouped %>% 
  group_by(month, year) %>% 
  summarise(rain = sum(rain, na.rm = T)) %>% 
  mutate(month_txt = paste(year, month, "01", sep = "-")) %>% 
  mutate(month_d = as.Date(month_txt)) %>%
  mutate(month_d = format(month_d, "%Y-%m")) %>% 
  ungroup() %>% 
  mutate(DOY = seq(15, 365, by = 365/12))



### compute statistics with a rolling window
library(zoo)

test <- 
  dataset_filtered %>% 
  arrange(date_time) %>% 
  group_by(date_julian, day, month, year) %>% 
  summarise(temp = mean(temp, na.rm = T),
            rain = sum(rain, na.rm = T),
            avg_wind_speed = mean(avg_wind_speed, na.rm = T),
            et = mean(et, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(rol_temp_mean = rollapply(data = temp, width = 15, 
                                   FUN = mean, align = "right", fill = NA, na.rm = TRUE),
         rol_rain_mean = rollapply(data = rain, width = 15, 
                                   FUN = sum, align = "right", fill = NA),
         rol_avg_wind_speed_mean = rollapply(data = avg_wind_speed, width = 15, 
                                             FUN = mean, align = "right", fill = NA),
         rol_aet = rollapply(data = et, width = 15, 
                                             FUN = mean, align = "right", fill = NA)) %>% 
  mutate(date = paste(year, month, day, sep = "-")) %>% 
  mutate(date = as.Date(date))


p <- ggplot(test, aes(x=date, y=rol_temp_mean)) +
  geom_line() + 
  xlab("") + 
  scale_x_date(date_labels = "%Y %b %d", date_breaks = "2 weeks") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p


p <- ggplot(test, aes(x=date, y=rol_avg_wind_speed_mean)) +
  geom_line() + 
  xlab("") + 
  scale_x_date(date_labels = "%Y %b %d", date_breaks = "2 weeks") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p


p <- ggplot(test, aes(x=date, y=rol_rain_mean)) +
  geom_line() + 
  xlab("") + 
  scale_x_date(date_labels = "%Y %b %d", date_breaks = "2 weeks") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p


p <- ggplot(test, aes(x=date, y=rol_aet)) +
  geom_line() + 
  xlab("") + 
  scale_x_date(date_labels = "%Y %b %d", date_breaks = "2 weeks") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p
