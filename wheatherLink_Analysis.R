############################################################################################################
############################################################################################################
############################################################################################################
###################
###################
###################           Compile & Explore meteo. data from Mbalmayo
###################
###################                                                                       03/09/2024
############################################################################################################
############################################################################################################
############################################################################################################


######
###### Read and compile data from www.weatherlink.com
######

#install.packages("reader")
library("reader")

path_to_data_export = "ENEF01_1-10-23_00-00_1_Year_1727701463_v2.csv"
path_to_data_export = "Bouamir_Research_Station_11-19-23_23-00_1_Year_1720206107_v2.csv"

# Prepare table header
head = n.readLines(path_to_data_export,
                   n = 1,
                   skip = 5)
head = strsplit(head, ",")[[1]]
name_list = c()
for(i in 1:length(head))
{
  name_i = substr(head[i], 2, nchar(head[i]))
  name_i = ifelse(length(strsplit(name_i, "- ")[[1]]) == 1,
                  substr(name_i, 0, nchar(name_i)-1),
                  substr(strsplit(name_i, "- ")[[1]][1], 0, nchar(strsplit(name_i, "- ")[[1]][1])-1)
                  )
  name_i = paste0(strsplit(name_i, " ")[[1]], collapse="_")
  name_list = c(name_list, name_i)
}

# Determine number of line
tmp = readLines(path_to_data_export)
n_lines = length(tmp)

# Compile table (line per line -> long -> 5-10 min)
compteur = 0
for(i in 6:n_lines)
{
compteur = compteur +1

tmp = n.readLines(path_to_data_export,
            n = 1,
            skip = i)

tmp = strsplit(tmp, '",\"')[[1]]
tmp = as.data.frame(t(tmp))
tmp[1,1] = substr(tmp[1,1], 2, nchar(tmp[1,1]))
tmp[1, ncol(tmp)] = substr(tmp[1, ncol(tmp)], 0, nchar(tmp[1, ncol(tmp)])-1)

if(compteur == 1) { res = tmp }
if(compteur >  1) { res = rbind(res, tmp) }
#print(compteur)
}
names(res) = name_list 

# replace commas by dots on numeric columns
names_col_character = c("Date_&_Time", "Prevailing_Wind_Dir", "Avg_Wind_Dir", "High_Wind_Direction")
for(i in 1:ncol(res))
{
if(names(res)[i] %in% names_col_character) { next }   
res[,i] = as.numeric(gsub(",", ".", gsub("\\.", "", res[,i])))
}

# backup check
res_backup = res
head(res)

# export
# saveRDS(res, "C:/Users/Pierre/Downloads/ENEF01_1-10-23_00-00_1_Year.rds")
# saveRDS(res, "C:/Users/Pierre/Downloads/Bouamir_Research_Station_11-19-23_23-00_1_Year.rds")

# res=readRDS("C:/Users/Pierre/Downloads/Bouamir_Research_Station_11-19-23_23-00_1_Year.rds")
# res=readRDS("C:/Users/Pierre/Downloads/ENEF01_1-10-23_00-00_1_Year.rds")

######
###### Data preprocessing  
######

require(lubridate)
require(data.table)
require(ggplot2)

# Reformat date & time info
names(res)[1] = "Date"
tmp = strsplit(res$Date, "/", fixed = TRUE)
formatDate =  ifelse(max(as.numeric(sapply(tmp, "[", 1))) == 12,
                     "%m/%d/%Y %H:%M",
                     "%d/%m/%Y %H:%M")

res$date = as.POSIXct(res$Date, format=formatDate)
res$Year <- year(res$date)
res$Month <- month(res$date)
res$Week <- week(res$date)
res$Day <- day(res$date)
res$Hour <- hour(res$date)
res$minute <- minute(res$date)

# Create a column with the full number of record supposidely present
id_min = which(res$date == min(res$date))[1]
id_max = which(res$date == max(res$date))[1]
full_record = seq(ymd_hm(paste0("00", res$Year[id_min], "-", res$Month[id_min], "-", res$Day[id_min], " ", res$Hour[id_min], ":", res$minute[id_min]), tz=""),
                  ymd_hm(paste0("00", res$Year[id_max], "-", res$Month[id_max], "-", res$Day[id_max], " ", res$Hour[id_max], ":", res$minute[id_max]), tz=""), 
                  by = '15 mins')
res$date[1] == full_record[1]
full_record = data.frame(theoretical_records = full_record)

nrow(res)
nrow(full_record)
res = merge(res, full_record, by.x = "date", by.y = "theoretical_records", all = T)
nrow(res)

# re-fill
res$Year <- year(res$date)
res$Month <- month(res$date)
res$Week <- week(res$date)
res$Day <- day(res$date)
res$Hour <- hour(res$date)
res$minute <- minute(res$date)

# check : data availablity
summary(res)
head(res)

check_NA_distrib = function(data = res, var_name = "Solar_Rad")
{
  Month_list = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  Year_color = c("23" = "darkblue", "24" = "darkgreen")
  
  names(data)[which(names(data) == var_name)] = "var_of_interest" 
  data.dt = as.data.table(data)
  tmp = data.dt[, by=c("Year", "Month"), .(number_of_NA = length(which(is.na(var_of_interest))),
                                           number_normally_recorded = length(date)
                                           )]
  tmp$Prop_missing = tmp$number_of_NA / tmp$number_normally_recorded 
  tmp$Month = as.factor(tmp$Month)
  tmp$Year = as.factor(tmp$Year)
  
  plot2beReturned = ggplot(data = tmp, aes(x = Month, y = Prop_missing, fill = Year)) +
                    geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
                    scale_x_discrete(breaks=1:12,
                                     labels = Month_list) +
                    scale_fill_manual(values = Year_color) +
                    xlab("") +
                    coord_polar(direction = 1) +
                    theme_light() +
                    ylab("Proportion of missing data") + 
                    theme(axis.text = element_text(size=14),
                          axis.title = element_text(size=14, face="bold")) + 
                    ggtitle(var_name) 
  
  return(plot2beReturned)
}

check_NA_distrib(data = res, var_name = "Solar_Rad")
check_NA_distrib(data = res, var_name = "Rain")
check_NA_distrib(data = res, var_name = "Avg_Wind_Speed")

# let's dig : how are missing data (of "Rain" column) distributed per day
Year_list = unique(res$Year)
for(Year in 1:length(Year_list))
{
  Month_list = unique(res$Month[which(res$Year == Year_list[Year])])
  for(Month in 1:length(Month_list))
  {
  Day_list =   unique(res$Day[which(res$Year == Year_list[Year] & 
                                    res$Month == Month_list[Month])])
  for(Day in 1:length(Day_list))
  {
    
    full_record = seq(ymd_hm(paste0("00", Year_list[Year], "-", Month_list[Month], "-", Day_list[Day], " 00:00"), tz=""),
                      ymd_hm(paste0("00", Year_list[Year], "-", Month_list[Month], "-", Day_list[Day], " 23:45"), tz=""), 
                      by = '15 mins')
    full_record = as.data.frame(full_record) ; names(full_record)[1] = 'date'
    tmp = res[which(res$Month == Month_list[Month] & res$Day == Day_list[Day] & res$Year == Year_list[Year]),]
    tmp = merge(tmp, full_record, by="date", all = T)
    ### Plot
    par(mfrow=c(1,2))
    # Rain
    plot(tmp$date, 1:nrow(tmp), 
         xlab = "Daily time sequence every 15 min",
         ylab = "Daily record number",
         las = 1,
         main = paste("Year : ", Year_list[Year], " | Month :", Month_list[Month], " | Day :", Day_list[Day]))
    id = which(!is.na(tmp$Rain))
    points(tmp$date[id], id, pch=19) # fuck
    legend("bottomright", legend=c("With data", "WithOUT data"), pch=c(19, 1), bty="n")
    legend("topleft", legend=c("Precipitation data"), bty="n")
    # Solar Rad
    plot(tmp$date, 1:nrow(tmp), 
         xlab = "Daily time sequence every 15 min",
         ylab = "Daily record number",
         las = 1,
         main = paste("Year : ", Year_list[Year], " | Month :", Month_list[Month], " | Day :", Day_list[Day]))
    id = which(!is.na(tmp$Solar_Rad))
    points(tmp$date[id], id, pch=19) # fuck
    legend("bottomright", legend=c("With data", "WithOUT data"), pch=c(19, 1), bty="n")
    legend("topleft", legend=c("Solar Radiation data"), bty="n")
    Sys.sleep(.5)
  }
}
  
}

######
###### Data visualization  
######

Month_list = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
Year_color = c("23" = "darkblue", "24" = "darkgreen")

res.dt[, by=c("Day"), .(Rain = sum(Rain, na.rm=T),
                        nb_record = length(Rain))]

data.Aprl = res.dt[which(res.dt$Month == 4),]
data.Aprl[, by=c("Day"), .(Rain = sum(Rain, na.rm=T),
                        nb_record = length(Rain))]
blaDay = data.Aprl[which(data.Aprl$Day == 19),]
nrow(blaDay)
plot(blaDay$Rain)
sum(blaDay$Rain)

###  Precipitation
res.dt = as.data.table(res)
res.dt = res.dt[, by=c("Year", "Month"), .(Rain = sum(Rain, na.rm=T))]
res.dt$Month = as.factor(res.dt$Month)
res.dt$Year = as.factor(res.dt$Year)

ggplot(data = res.dt, aes(x = Month, y = Rain, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
  scale_x_discrete(breaks=1:12,
                   labels = Month_list) +
  scale_fill_manual(values = Year_color) +
  xlab("") +
  coord_polar(direction = 1) +
  theme_light() +
  ylab("Rain (mm)") + 
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14, face="bold"))


###  average Solar Radiation (kWh / m² / day : [3 ; 7])
res.dt = as.data.table(res)
res.dt = res.dt[, by=c("Year", "Month", "Day", "Hour"), .(Solar_Rad = sum(Solar_Rad, na.rm=T))]
res.dt = res.dt[, by=c("Year", "Month"), .(Solar_Rad = mean(Solar_Rad, na.rm=T))]
res.dt$Month = as.factor(res.dt$Month)
res.dt$Year = as.factor(res.dt$Year)

ggplot(data = res.dt, aes(x = Month, y = Solar_Rad, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
  scale_x_discrete(breaks=1:12,
                   labels = Month_list) +
  scale_fill_manual(values = Year_color) +
  xlab("") +
  coord_polar(direction = 1) +
  theme_light() +
  ylab("Solar_Rad") + 
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14, face="bold"))





###  Avg_Wind_Speed
res.dt = as.data.table(res)
res.dt = res.dt[, by=c("Year", "Month"), .(Avg_Wind_Speed = sum(Avg_Wind_Speed, na.rm=T))]
res.dt$Month = as.factor(res.dt$Month)
res.dt$Year = as.factor(res.dt$Year)

ggplot(data = res.dt, aes(x = Month, y = Avg_Wind_Speed, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
  scale_x_discrete(breaks=1:12,
                   labels = Month_list) +
  scale_fill_manual(values = Year_color) +
  xlab("") +
  coord_polar(direction = 1) +
  theme_light() +
  ylab("Avg_Wind_Speed") + 
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14, face="bold"))









require(ggplot2)

meteo2023 = read.table("C:/Users/Pierre/Downloads/ENEF_meteo_2023.csv",
                       sep=";", dec=",", header=T)

str(meteo2023)
plot(meteo2023$Rain)

meteo2023$date = as.Date(meteo2023$Date)
str(meteo2023)


#  Get months
meteo2023$Month <- months(meteo2023$date)

#  Aggregate 'X2' on months and get mean
datatemp = aggregate( Rain ~ Month , meteo2023 , sum )
datatemp = datatemp[-which(datatemp$Month=="juin"),]
# datatemp[10,] = NA
# datatemp$Month[10] = "juillet"
# datatemp[11,] = NA
# datatemp$Month[11] = "août"
# datatemp[12,] = NA
# datatemp$Month[12] = "septembre"

datatemp$Month = factor(datatemp$Month, 
                        levels=c("octobre","novembre","décembre", "janvier","février","mars","avril","mai"))


ggplot(data = datatemp, aes(x = Month, y = Rain)) +
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE, fill="blue4") +
  coord_polar(direction = 1) +
  theme_light() +
  xlab("") + 
  ylab("Pluie (mm)")


tot = 2 / 12
juin = 6 * tot
sept = 9 * tot
ggplot(data = datatemp, aes(x = Month, y = Rain)) +
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE, fill="blue4") +
  coord_radial(start = sept * pi, end = juin * pi) +
  xlab("") +
  ylab("Precipitations mensuelles (mm)")


######
###### Solar radiations
######

#  Aggregate 'X2' on months and get mean
datatemp = aggregate( SolarRad ~ Month , meteo2023 , sum )
datatemp = datatemp[-which(datatemp$Month=="juin"),]
# datatemp[10,] = NA
# datatemp$Month[10] = "juillet"
# datatemp[11,] = NA
# datatemp$Month[11] = "août"
# datatemp[12,] = NA
# datatemp$Month[12] = "septembre"

datatemp$Month = factor(datatemp$Month, 
                        levels=c("octobre","novembre","décembre", "janvier","février","mars","avril","mai"))


ggplot(data = datatemp, aes(x = Month, y = SolarRad)) +
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE, fill="yellow4") +
  coord_polar(direction = 1) +
  theme_light() +
  xlab("") + 
  ylab("Pluie (mm)")


tot = 2 / 12
juin = 6 * tot
sept = 9 * tot
ggplot(data = datatemp, aes(x = Month, y = SolarRad)) +
  geom_bar(stat = "identity", position = "dodge",na.rm = FALSE, fill="yellow4") +
  coord_radial(start = sept * pi, end = juin * pi) +
  xlab("") +
  ylab("Cumulated solar radiations (W/m²)")



