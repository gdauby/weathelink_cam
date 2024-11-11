
library(tidyverse)
library(dplyr)
library("imputeTS")
library("climatol") # https://climatol.eu/

require(data.table)
source("functions_pp.R")

#install.packages("climaemet", repos = c("https://ropenspain.r-universe.dev", "https://cloud.r-project.org"))
require(climaemet)

library(tidyverse)
library(ggridges)
library(scales)


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
diagwl(tmpx,
       cols=NULL, 
       est="Mbalmayo", 
       alt=600, 
       per="2023-2024", 
       mlab="en", 
       shem=T)


ggclimat_walter_lieth(tmpx,
                      alt = "600", per = "2023-2024",
                      est = "Mbalmayo")



############
############  Temperature
############

###
### Look at Temperature distribution
###

# Plot T° considering all raw (NA omited) records
{
#.......................calculate avg temp.......................
T_avg <- dataset_num |> 
  summarize(mean_temp = round(mean(temp, na.rm = TRUE), 1)) |> 
  pull() 

#..............................plot..............................
dataset_num |> 
  group_by(month) |> 
  
  # initialize ggplot ----
ggplot(aes(x = temp, y = month, fill = after_stat(x))) +
  
  # create ridgeline plot ----
ggridges::geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) + # rel_min_height cuts trailing tails (0.01 suggested); scale sets extent of overlap
  
  # add vertical line at avg temp + annotation
  geom_vline(xintercept = T_avg, linetype = "dashed", color = "black") +
  
  # set x-axis breaks ----
scale_x_continuous(breaks = seq(15,40,5)) +
  
  # arrange months in reverse chronological order ----
scale_y_discrete(labels = month.name) + 
  
  # fill color ----
scale_fill_gradientn(colors = c("#2C5374","#778798", "#ADD8E6", "#EF8080", "#8B3A3A"), name = "Temp. (°C)") +
  
  # update labs & titles ----
labs(x = "Temperature (°C)") +
  
  # apply theme ----
ggridges::theme_ridges(font_size = 13, grid = TRUE) +
  
  # adjust theme options ----
theme(
  axis.title.y = element_blank()
)
}

# Plot considering all aggregated daily records (-> here, max daily T°)
{
data.dt = as.data.table(data)
data.dt = data.dt[, by=c("month", "date_julian_Day", "day"), .("MeanT" = mean(temp, na.rm = T))]
#.......................calculate avg temp.......................
T_avg <- data.dt |> 
  summarize(mean_temp = round(mean(MeanT, na.rm = TRUE), 1)) |> 
  pull() 

#..............................plot..............................
data.dt |> 
  group_by(month) |> 
  
  # initialize ggplot ----
ggplot(aes(x = MeanT, y = as.factor(month), fill = after_stat(x))) +
  
  # create ridgeline plot ----
ggridges::geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) + # rel_min_height cuts trailing tails (0.01 suggested); scale sets extent of overlap
  
  # add vertical line at avg temp + annotation
  geom_vline(xintercept = T_avg, linetype = "dashed", color = "black") +
  
  # set x-axis breaks ----
scale_x_continuous(breaks = seq(15,40,5)) +
  
  # arrange months in reverse chronological order ----
scale_y_discrete(labels = month.name) + 
  
  # fill color ----
scale_fill_gradientn(colors = c("#2C5374","#778798", "#ADD8E6", "#EF8080", "#8B3A3A"), name = "Temp. (°C)") +
  
  # update labs & titles ----
labs(x = "Temperature (°C)") +
  
  # apply theme ----
ggridges::theme_ridges(font_size = 13, grid = TRUE) +
  
  # adjust theme options ----
theme(
  axis.title.y = element_blank()
)
}

# Plot heatmap
ggplot(data = data.dt, aes(x = day, y = as.factor(month))) + 
  geom_tile(aes(fill = MeanT)) + 
  coord_equal(ratio = 1) + 
  ylab("Month") + 
  scale_fill_viridis(option="magma") 

# Heatmap v2
month.labs <- month.name
names(month.labs) <- 1:12
ggplot(data, aes(x = day, y = hour, fill = temp)) +
  geom_tile() +
  scale_fill_viridis(name = expression(degree*C),
                     option = "plasma") +
  facet_grid(cols = vars(month), labeller = labeller(month = month.labs)) +
  ylab("Hour of the day") +
  xlab("Day of the week") 

# Circular style
dataset_num$date_tmp = format(as.POSIXct(dataset_num$date),format='%m/%d')  # remove hour:min
data.dt = as.data.table(dataset_num)
Temp.dt = data.dt[, by=c("date_tmp"), .("min_temperaturec" = min(temp, na.rm = T),      
                                        "max_temperaturec" = max(temp, na.rm = T),         
                                        "mean_temperaturec" = mean(temp, na.rm = T))]
Temp.dt$date_tmp = as.Date(as.POSIXct(Temp.dt$date_tmp,format='%m/%d'))

ggplot(Temp.dt, aes(date_tmp, ymin = min_temperaturec, ymax = max_temperaturec, color = mean_temperaturec)) +
  geom_linerange(size = 1.3, alpha = 0.75) +
  scale_color_viridis_c(NULL, option = "A") +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("month")) +
  ylim(15, 35) +
  labs(
    title = "T° circular style",
    x = NULL,
    y = NULL) +
  coord_polar() +
  theme(legend.position = "bottom") + 
  theme_light()

############
############  Wind analyses
############

# Convert cardinal wind directions to degrees (numeric)
data$avg_wind_dir_num = cardinal_direction_to_degrees(data$avg_wind_dir)
data$high_wind_direction_num = cardinal_direction_to_degrees(data$high_wind_direction)

# Check what data we playing with
head(data[,which(grepl(names(data), pattern = "wind"))])

# Convert/aggregate records into daily averages
wind_data = as.data.table(data)
wind_data = wind_data[, by=c("month", "date_julian_Day"), .("meanDaily_avg_wind_speed" = mean(avg_wind_speed, na.rm = T),       
                                                            "meanDaily_avg_wind_dir_num" = mean(avg_wind_dir_num, na.rm = T),         
                                                            "maxDaily_high_wind_speed" = max(high_wind_speed, na.rm = T),         
                                                            "meanDaily_high_wind_direction_num" = mean(high_wind_direction_num, na.rm = T)  
                                                            )]

# Convert/aggregate records from daily to monthly averages
wind_data = wind_data[, by=c("month"), .("meanMonthly_avg_wind_speed" = mean(meanDaily_avg_wind_speed, na.rm = T),       
                                         "meanMonthly_avg_wind_dir_num" = mean(meanDaily_avg_wind_dir_num, na.rm = T),         
                                         "meanMonthly_high_wind_speed" = mean(maxDaily_high_wind_speed, na.rm = T),       
                                         "maxMonthly_high_wind_speed" = max(maxDaily_high_wind_speed, na.rm = T),
                                         "meanMonthly_high_wind_direction_num" = mean(meanDaily_high_wind_direction_num, na.rm = T)  
                                        )]

# plot annual wind synthesis focused on direction, w/o notion of time/month

ggwindrose(
  speed = wind_data$meanMonthly_avg_wind_speed, 
  direction = wind_data$meanMonthly_avg_wind_dir_num,
  speed_cuts = seq(0, 16, 4), legend_title = "Wind speed (m/s)",
  calm_wind = 0, n_col = 1, plot_title = "Mbalmayo")               # -> always same direction for average winds
    
ggwindrose(
  speed = wind_data$meanMonthly_high_wind_speed, 
  direction = wind_data$meanMonthly_high_wind_direction_num,
  speed_cuts = seq(0, 16, 4), legend_title = "Wind speed (m/s)",
  calm_wind = 0, n_col = 1, plot_title = "Mbalmayo")               # -> always same direction for high winds

ggwindrose(
  speed = wind_data$maxMonthly_high_wind_speed, 
  direction = wind_data$meanMonthly_high_wind_direction_num,
  speed_cuts = seq(0, 16, 4), legend_title = "Wind speed (m/s)",
  calm_wind = 0, n_col = 1, plot_title = "Mbalmayo")               # -> always same direction for high winds

# plot annual synthesis focused on seasonal variation

ggplot(data = wind_data,
       aes(x = month,
           y = meanMonthly_avg_wind_speed)) +
  geom_col() + 
  coord_polar(theta = "x") + 
  scale_x_continuous(breaks = 1:13,
                   labels = c(month.abb[1:12], "NA")) +
  theme_minimal() +
  theme(legend.position="none") +
  xlab("") + 
  ylab("Average wind speed")


ggplot(data = wind_data,
       aes(x = month,
           y = maxMonthly_high_wind_speed)) +
  geom_col() + 
  coord_polar(theta = "x") + 
  scale_x_continuous(breaks = 1:13,
                     labels = c(month.abb[1:12], "NA")) +
  theme_minimal() +
  theme(legend.position="none") +
  xlab("") + 
  ylab("High wind speed")








# Convert/aggregate records into daily averages
wind_data = as.data.table(data)
wind_data = wind_data[, by=c("month","week", "date_julian_Day"), .("meanDaily_avg_wind_speed" = mean(avg_wind_speed, na.rm = T),       
                                                            "meanDaily_avg_wind_dir_num" = mean(avg_wind_dir_num, na.rm = T),         
                                                            "maxDaily_high_wind_speed" = max(high_wind_speed, na.rm = T),         
                                                            "meanDaily_high_wind_direction_num" = mean(high_wind_direction_num, na.rm = T),
                                                            "Precip" = sum(rain, na.rm = T)       # total daily precipitation
)]

# Convert/aggregate records from daily to monthly averages
wind_data = wind_data[, by=c("month","week"), .("meanMonthly_avg_wind_speed" = mean(meanDaily_avg_wind_speed, na.rm = T),       
                                         "meanMonthly_avg_wind_dir_num" = mean(meanDaily_avg_wind_dir_num, na.rm = T),         
                                         "meanMonthly_high_wind_speed" = mean(maxDaily_high_wind_speed, na.rm = T),       
                                         "maxMonthly_high_wind_speed" = max(maxDaily_high_wind_speed, na.rm = T),
                                         "meanMonthly_high_wind_direction_num" = mean(meanDaily_high_wind_direction_num, na.rm = T),
                                         "Precip" = sum(Precip, na.rm = T)        # total monthly precipitation
)]


ggplot(data = wind_data,
       aes(x = week,
           y = Precip)) +
  geom_col(fill="blue4") + 
  scale_x_continuous(breaks = 1:53) +
  theme_minimal() +
  theme(legend.position="none") +
  xlab("") + 
  ylab("High wind speed") +
  geom_line(data = wind_data, aes(x = week, y = maxMonthly_high_wind_speed*20), size=1.5, col="red") +
  coord_polar(theta = "x") 



ggplot(data = wind_data,
       aes(x = as.factor(month),
           y = Precip)) +
  #coord_polar(theta = "x") + 
  geom_col( aes(x = week/4,
                y = Precip), fill="blue4") 
  




  ggplot() +
  geom_bar( data = wind_data, aes(x=week, y=Precip, group= month, color=month)) 












ggclimat_walter_lieth_pp(tmpx,
                      alt = "600", per = "2023-2024",
                      est = "Mbalmayo")


min_ylim = 0
max_ylim = 40

dat_long <- tibble::as_tibble(as.data.frame(t(tmpx)))
dat_long$month = 1:12
dat_long$prescal = scales::rescale(dat_long$Precip,
                                  from = range(dat_long$Precip),
                                  to = c(min_ylim, max_ylim))
  
pcol = "#002F70", 
tcol = "#ff0000"

ggplot() + 
  geom_line(data = dat_long, aes(x = month, y = MaxT), color = tcol) + 
  ylim(min_ylim,max_ylim) + 
  geom_line(data = dat_long, aes(x = month, y = prescal), color = pcol)





Temp_expansion_factor = 1.2

range_tmp = range(dat_long$MaxT)
dat_long$MaxT_rescale = scales::rescale(dat_long$MaxT,
                                        from = range_tmp,
                                        to = c(range_tmp[1]/Temp_expansion_factor, range_tmp[2]*Temp_expansion_factor))



min_first  <- min(dat_long$MaxT_rescale)   # Specify min of first y axis : T°
max_first  <- max(dat_long$MaxT_rescale)   # Specify max of first y axis : T°
min_second <- min(dat_long$Precip) # Specify min of second y axis: Rain 
max_second <- max(dat_long$Precip) # Specify max of second y axis : Rain



# scale and shift variables calculated based on desired mins and maxes
scale = 10 #(max_second - min_second)/(max_first - min_first)
shift = min_first - min_second

# Function to scale secondary axis
scale_function <- function(x, scale, shift){
  return ((x)*scale - shift)
}

# Function to scale secondary variable values
inv_scale_function <- function(x, scale, shift){
  return ((x + shift)/scale)
}

ggplot(dat_long, aes(x = month, y = MaxT_rescale)) +
  geom_line(aes(color = "Temperature")) +
  geom_line(aes(y = inv_scale_function(Precip, scale, shift), color = "Rain")) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift), name="Rain (mm)")) 
  








