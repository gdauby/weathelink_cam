

library(tidyverse)

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

dataset %>% slice(1:10) %>% View

dataset %>% 
  mutate(inside_temp = str_replace(inside_temp, ",", ".")) %>% 
  mutate(inside_temp = as.numeric(inside_temp))

dataset_num <- 
  dataset %>% 
  mutate(across(2:54, ~ na_if(., "--"))) %>%
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
  mutate(date_census_txt = ## reformater la date du census 1 pour convertir en jour julien
                             paste(ifelse(!is.na(month),
                                          month, 1), # if day is missing, by default 1
                                   ifelse(!is.na(day),
                                          day, 1), # if month is missing, by default 1
                                   ifelse(!is.na(year),
                                          year, ""),
                                   sep = "/")) %>%
  mutate(date_julian = date::as.date(date_census_txt)) %>% 
  relocate(date_julian, .after = date_time) ### date en julien

### table with number of NA and number for TEMPERATURE
missing_val_per_day <- 
  dataset_num %>% 
  group_by(date_julian, day, month, year) %>% 
  mutate(time_int = abs(date[2] - date[1])) %>% 
  summarise(nbe_na = sum(is.na(rain)),
            nbe_val = n(),
            time_int = min(time_int)) %>% 
  mutate(prop_missing_val = round(nbe_na/nbe_val*100, 1))

### table with time interval for each day and the maximam number of values it means
interval_measures <- 
  missing_val_per_day %>% 
  group_by(time_int) %>% 
  summarise(nbe = n(),
            max_val = max(nbe_val)) %>% 
  ungroup()

### adding the time interval for each day and compute the proportion of values indeed measured for each day
missing_val_per_day <- 
  missing_val_per_day %>% 
  left_join(interval_measures %>% 
              select(max_val, time_int)) %>% 
  mutate(prop_val_measured = round(nbe_val / max_val*100, 1))

### list of days for which we have less than 80 % of values
missing_val_per_day %>% 
  filter(prop_val_measured < 80)

missing_val_per_day %>% 
  filter(prop_val_measured < 90)


