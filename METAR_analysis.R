
####################
####################
####################

require(pmetar)
require(tidyverse)
require(data.table)

# from iastate
dm <- metar_get_historical("NSI", start_date = "2023-01-01", end_date = "2024-09-30", from = "iastate")
dm

# from ogimet
start_date = as.Date("2023-10-01")
end_date = as.Date("2024-09-30")
date_list = seq(from = start_date, to = end_date, by = 'month')
for(i in 1:(length(date_list)-1))
{
  tmp = compteur = 1
  while(length(tmp) == 1)
  { tmp = try(metar_get_historical("NSI", start_date = date_list[i], end_date = (date_list[i+1]-1), from = "ogimet"), silent=T) ; 
  compteur = compteur + 1
  print(paste("Retrieving trial n°", compteur))
  }
  if(i == 1) { dm2 = tmp }
  if(i > 1 ) { dm2 = c(dm2, tmp) }
  print(i)
}

dataNSI_v1 = metar_decode(dm)
head(dataNSI_v1)
nrow(dataNSI_v1)

dataNSI_v2 = metar_decode(dm2)
head(dataNSI_v2)
nrow(dataNSI_v2)

# Are there some metar in v2 not available in v1?
id_to_retrieve = which(dataNSI_v2$METAR_Date %!in% dataNSI_v1$METAR_Date)
id_to_retrieve
dataNSI = rbind(dataNSI_v1, dataNSI_v2[id_to_retrieve,])

dataNSI=dataNSI_v1
# Remove NAs
dateNA = which(is.na(dataNSI$METAR_Date) == T)
dataNSI[dateNA,]
dataNSI = dataNSI[-dateNA,]

# Convert date & add date-related columns
dataNSI <- dataNSI %>% 
  mutate(date = as.POSIXct(METAR_Date, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         week = week(date),
         hour = hour(date),
         minute = minute(date))

dataNSI$date_julian <-  julian(dataNSI$date, 
                               origin = min(dataNSI$date))

dataNSI = dataNSI[order(dataNSI$date),]
head(dataNSI[, c("METAR_Date", "date", "date_julian")])

# Recode qualitative Precipitation info into binary data (0/1)
id_noRain = which(grepl(dataNSI$Weather_information, pattern = "Rain") == F)
id_rainLIGHT = which(grepl(dataNSI$Weather_information, pattern = "Light intensity") == T & 
                       grepl(dataNSI$Weather_information, pattern = "Rain") == T)
id_rainHEAVY = which(grepl(dataNSI$Weather_information, pattern = "Heavy intensity") == T & 
                       grepl(dataNSI$Weather_information, pattern = "Rain") == T)
id_rainMODERATE = which(grepl(dataNSI$Weather_information, pattern = "Rain") == T)
id_rainMODERATE = id_rainMODERATE[which(grepl(dataNSI$Weather_information[id_rainMODERATE], pattern = "Heavy") == F)]
id_rainMODERATE = id_rainMODERATE[which(grepl(dataNSI$Weather_information[id_rainMODERATE], pattern = "Light") == F)]
dataNSI$Preci_0 = dataNSI$Preci_1 = dataNSI$Preci_2 = dataNSI$Preci_3 = 0
dataNSI$Preci_0[id_noRain] = dataNSI$Preci_1[id_rainLIGHT] = dataNSI$Preci_2[id_rainMODERATE] = dataNSI$Preci_3[id_rainHEAVY] = 1


dataNSI.summary = data.table(dataNSI)
dataNSI.summary = dataNSI.summary[, by=c("year", "month", "week", "day"), .(N_records = length(Airport_ICAO))]
dataNSI.summary

GenerateDailyPlot = function(data.tmp = data.tmp)
{
  # Create base plot
  data.daily.theo = data.frame(hour=0:24)
  p_base = ggplot(data.daily.theo, aes(x = hour, y = 1)) +
    geom_point(col="NA") + 
    coord_polar(theta = "x") +
    scale_x_continuous(breaks = 0:25) +
    coord_polar() +  
    theme_minimal() + xlab("") + ylab("") +
    theme(axis.text=element_text(size=12),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +  
    theme(legend.position="none")
  
  # add daily records
  data.tmp$hour_fractionnal = data.tmp$hour + data.tmp$minute/60
  p_fin = p_base + geom_point(data = data.tmp, mapping = aes(x = hour_fractionnal, y = 1))   
  
  # return plot
  return(p_fin)
}


for(i in 1:nrow(dataNSI.summary))
{
data.tmp = dataNSI[which(dataNSI$year == dataNSI.summary$year[i] &
                         dataNSI$month == dataNSI.summary$month[i] &
                         dataNSI$day == dataNSI.summary$day[i]),]

# Prepare table of results
dailyData.summary = data.frame(year = data.tmp$year[1],
                               month = data.tmp$month[1],
                               day = data.tmp$day[1],
                               N_records = nrow(data.tmp))
  
# Generate plot
GenerateDailyPlot(data.tmp)
    
# Detect temporal resolution(s) & their mode in the raw data 
diff_fromOrigin_InDays = julian(data.tmp$date, origin = min(data.tmp$date[1]))
diff_fromOrigin_InMinutes = diff_fromOrigin_InDays*24*60
Temporal_resolution_InMinutes = diff(diff_fromOrigin_InMinutes)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
dailyData.summary$TempRes_mode_min = Mode(Temporal_resolution_InMinutes)
dailyData.summary$TempRes_N = length(unique(Temporal_resolution_InMinutes))

# Aggregate data per hour
data.tmp = merge(data.tmp, data.frame(hour=0:23), by="hour", all=T)
data.tmp.dt = data.table(data.tmp)
data.tmp.dt = data.tmp.dt[, by=c("year", "month", "week", "day", "hour"), .(Temperature = mean(Temperature, na.rm=T),
                                                                            Wind_speed_mean = mean(Wind_speed, na.rm=T),
                                                                            Wind_speed_max = max(Wind_speed, na.rm=T),
                                                                            Preci_0 = mean(Preci_0, na.rm=T),
                                                                            Preci_1 = mean(Preci_1, na.rm=T),
                                                                            Preci_2 = mean(Preci_2, na.rm=T),
                                                                            Preci_3 = mean(Preci_3, na.rm=T)
                                                                            )]

# Detect largest data gap within the day once aggregated per hour & amount of missing values (out of 24)
x = ifelse(is.na(data.tmp.dt$Temperature), 1, 0)
d = diff(c(0, x, 0))
t = seq(1, nrow(data.tmp.dt), 1)
TS_NA = data.frame(on_t = t[d == 1], off_t = t[(d == -1)[-1]])
TS_NA$length = TS_NA$off_t - TS_NA$on_t + 1
dailyData.summary$dataGap_max_hour = ifelse(nrow(TS_NA) == 0, 0, max(TS_NA$length, na.rm=T))
dailyData.summary$dataGap_cum_hour = length(which(x==1))

# Store hourly data
if(i == 1) { data.all.dt = data.tmp.dt ; dailyData.all.summary = dailyData.summary }
if(i >  1) { data.all.dt = rbind(data.all.dt, data.tmp.dt) ; dailyData.all.summary = rbind(dailyData.all.summary, dailyData.summary)}
print(i)
}

head(data.all.dt)
summary(data.all.dt)

head(dailyData.all.summary)
summary(dailyData.all.summary)

###
### Check data as a whole
###

hist(dailyData.all.summary$dataGap_max_hour, breaks=50, main = "Taille max des plages de données manquantes par jour", xlab="en heure")
hist(dailyData.all.summary$dataGap_cum_hour, breaks=50, main = "Taille cumulées des plages de données manquantes par jour", xlab="en heure")
plot(dailyData.all.summary$dataGap_max_hour, dailyData.all.summary$dataGap_cum_hour)

###
### Check individual days
###

head(dailyData.all.summary)

# Select data of days of interest
data.tmp = dataNSI[which(dataNSI$year == 2023 &
                           dataNSI$month == 3 &
                           dataNSI$day == 4),]

# Generate plot
GenerateDailyPlot(data.tmp)

###
### Filter out some days with too much NAs
###

# Filtrer sur la base de la durée de la plus grande plage de données manquante au sein de la journée
hist(dailyData.all.summary$dataGap_max_hour, breaks=50, main = "Taille max des plages de données manquantes par jour", xlab="en heure")
dataGap_max_hour = 5 # un trou de plus de 2h ==> OUT
(length(which(dailyData.all.summary$dataGap_max_hour > dataGap_max_hour)) / nrow(dailyData.all.summary))*100 # -> % de données exclue

# Filtrer sur la base de la durée cumulée des plages de données manquante au sein de la journée
hist(dailyData.all.summary$dataGap_cum_hour, breaks=50, main = "Taille cumulées des plages de données manquantes par jour", xlab="en heure")
dataGap_cum_hour = 8 # une quantité cumulée manquante de plus de 5h dans la journée ==> OUT
(length(which(dailyData.all.summary$dataGap_cum_hour > dataGap_cum_hour)) / nrow(dailyData.all.summary))*100 # -> % de données exclue

# Combiner les 2 filtres précédent
id_tobe_removed = which(dailyData.all.summary$dataGap_max_hour > dataGap_max_hour | 
                        dailyData.all.summary$dataGap_cum_hour >= dataGap_cum_hour )
(length(id_tobe_removed) / nrow(dailyData.all.summary))*100 # -> % de données exclue

# save filter results
dailyData.all.summary$toBeFiltered = 0
dailyData.all.summary$toBeFiltered[id_tobe_removed] = 1

###
### Prepare filtered datasets
###

tmp = dailyData.all.summary[which(dailyData.all.summary$toBeFiltered==0),]
tmp$YMD = paste0(tmp$year, "_", tmp$month, "_", tmp$day)

dataNSI$YMD = paste0(dataNSI$year, "_", dataNSI$month, "_", dataNSI$day)
nrow(dataNSI)
dataNSI = dataNSI[which(dataNSI$YMD %in% tmp$YMD),]
nrow(dataNSI)

data.all.dt$YMD = paste0(data.all.dt$year, "_", data.all.dt$month, "_", data.all.dt$day)
nrow(data.all.dt)
data.all.dt = data.all.dt[which(data.all.dt$YMD %in% tmp$YMD),]
nrow(data.all.dt)
data.all.dt$date = paste0(data.all.dt$year, "-", data.all.dt$month, "-", data.all.dt$day, " ", data.all.dt$hour, ":30")
data.all.dt$date = as.POSIXct(data.all.dt$date)
data.all.dt$date_julian <-  julian(data.all.dt$date, origin = min(data.all.dt$date))

######
###### Explore Temperatures on FILTERED dataset
######

tmp = data.all.dt[which(data.all.dt$week == 43 & data.all.dt$year == 2023),]
plot(tmp$date, tmp$Temperature, pch=21, bg="grey", cex=1.2)
points(tmp$date, tmp$Temperature, type="l")

tmpmbal = dataset_num[which(dataset_num$week == 43 & dataset_num$year == 2023),]
points(tmpmbal$date, tmpmbal$temp, pch = 21, bg="red")

# ...

require(scales)
# Circular style

tmp$date[1:10]
format(as.POSIXct(tmp$date[1:10]),format='%m/%d')

Temp.dt$date_tmp[1:10]
as.POSIXct(Temp.dt$date_tmp[1:10],format='%m/%d')
as.Date(as.POSIXct(Temp.dt$date_tmp[1:10],format='%m/%d'))

tmp = data.all.dt[which(data.all.dt$date < "2023-10-30"),]
tmp$date_tmp = format(as.POSIXct(tmp$date),format='%m/%d')  # remove hour:min
data.dt = as.data.table(tmp)
Temp.dt = data.dt[, by=c("date_tmp"), .("min_temperaturec" = min(Temperature, na.rm = T),      
                                        "max_temperaturec" = max(Temperature, na.rm = T),         
                                        "mean_temperaturec" = mean(Temperature, na.rm = T))]
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


######
###### Explore Pluvio on FILTERED dataset
######

###
### Establish a model to predict quantitative precipitations based on qualitative values
###

data.all.dt$index = 1
NSI_sumaryByDay = data.all.dt %>% 
  group_by(year, week, day) %>% 
  summarise_at(c("Preci_0", "Preci_1", "Preci_2", "Preci_3", "index"), sum)

Mbal_sumaryBDay = dataset_num %>% 
  group_by(year, week, day) %>% 
  summarise_at(c("rain"), sum)

tmp = merge(NSI_sumaryByDay, Mbal_sumaryBDay, by=c("year", "week", "day"))

idLocalRain1 = which((tmp$Preci_1 + tmp$Preci_2 + tmp$Preci_3) == 0 & tmp$rain != 0) # il pleut pas à l'aeroport mais il pleut à l'ENEF
idLocalRain2 = which((tmp$Preci_1 + tmp$Preci_2 + tmp$Preci_3) != 0 & tmp$rain == 0) # il pleut à l'aeroport mais il pleut pas à l'ENEF
idLocalRain3 = which(tmp$Preci_0 == tmp$index & tmp$rain > 5) # il pleut pasà l'aerport (Preci_0 == index) et y'a plus de 5 mm de pluis à l'ENEF...
idLocalRain = unique(c(idLocalRain1, idLocalRain2, idLocalRain3))

tmpSub = tmp[-idLocalRain,] 

plot(tmpSub$rain, tmpSub$Preci_0)
plot(tmpSub$rain, tmpSub$Preci_1)

mod = lm(rain ~ -1 + Preci_0 + I(Preci_0^2) + 
           Preci_1 + I(Preci_1^2) + 
           Preci_2 + I(Preci_2^2) + 
           Preci_3 + I(Preci_3^2) , data = tmpSub)
summary(mod)

plot(tmpSub$rain, predict(mod))
abline(0,1)

###
### Predict quantitative precipitations based on qualitative values & plot
###

hist(data.all.dt$Preci_0)
hist(data.all.dt$Preci_1)
hist(data.all.dt$Preci_2)
hist(data.all.dt$Preci_3)

data.all.dt$rain_predicted = predict(mod, newdata = data.all.dt)
summary(data.all.dt$rain_predicted)
hist(data.all.dt$rain_predicted, breaks=50)
id_crazy = which(data.all.dt$rain_predicted > 10000)
data.all.dt[id_crazy,]
data.all.dt$rain_predicted[which(data.all.dt$Preci_0 == 1)] = 0

plot(data.all.dt$Preci_0, data.all.dt$rain_predicted)
plot(data.all.dt$Preci_1, data.all.dt$rain_predicted)
plot(data.all.dt$Preci_2, data.all.dt$rain_predicted)
plot(data.all.dt$Preci_3, data.all.dt$rain_predicted)


data.all.dt$rain_predicted[which(data.all.dt$rain_predicted > 100)] = 100



data.all.dt$date_julian_Day = as.integer(data.all.dt$date_julian)

data.all.dt.tmp = data.all.dt[, by=c("month", "date_julian_Day"), .("Precip" = sum(rain_predicted, na.rm = T),       # total daily precipitation
                                                              "MaxT" = max(Temperature, na.rm = T),         # maximum daily temperature
                                                              "MinT" = min(Temperature, na.rm = T),         # minimum daily temperature
                                                              "AbsMinT" = min(abs(Temperature), na.rm = T)  # Absolute minimum daily temperature
)]

data.all.dt.tmp = data.all.dt.tmp[, by=c("month"), .("Precip" = sum(Precip, na.rm = T),        # total monthly precipitation
                                           "MaxT" = mean(MaxT, na.rm = T),           # mean maximum daily temperature
                                           "MinT" = mean(MinT, na.rm = T),           # mean minimum daily temperature
                                           "AbsMinT" = mean(abs(AbsMinT), na.rm = T) # mean Absolute minimum daily temperature
)]


data.all.dt.tmp = data.all.dt.tmp[order(data.all.dt.tmp$month),]
tmpxNSI = as.data.frame(t(data.all.dt.tmp[,-1]))
data(climatol_data)
names(tmpxNSI) = names(datcli)

ggclimat_walter_lieth(tmpxNSI,
                      alt = "600", per = "2023-2024",
                      est = "Mbalmayo")



ggclimat_walter_lieth(tmpxNSI,
                      alt = "600", per = "2023-2024",
                      est = "Mbalmayo")





