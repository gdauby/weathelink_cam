
`%!in%` = Negate(`%in%`)

regularize_TS.df = function(data = dataset_num, tempRes = tempRes)
{
  
  # 1. Generate a theoretical time-series with no missing data and a tempRes time resolution
  full_record = data.frame(date = seq(min(dataset_num$date), max(dataset_num$date), by = tempRes))
  
  # 2. Merge incomplete TS of observed data with theoretical records  
  full_record$date = xts::align.time(full_record$date, 1)   # we round up to the next second
  dataset_num$date = xts::align.time(dataset_num$date, 1)   # we round up to the next second
  data = left_join(full_record, dataset_num, by=c("date"))
  head(data)
  
  return(data)
}

####
check_NA_distrib = function(data = dataset_num, var_name = "rain", tempRes = "15 min")
{
  
  # # 1. Generate a theoretical time-series with no missing data and a tempRes time resolution
  # full_record = data.frame(date = seq(min(dataset_num$date), max(dataset_num$date), by = tempRes))
  # 
  # # 2. Merge incomplete TS of observed data with theoretical records  
  # full_record$date = xts::align.time(full_record$date, 1)   # we round up to the next second
  # dataset_num$date = xts::align.time(dataset_num$date, 1)   # we round up to the next second
  # data = left_join(full_record, dataset_num, by=c("date"))
  # head(data)
  
  data = regularize_TS.df(data = dataset_num, tempRes = tempRes)
  
  # Create graph
  Month_list = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  Year_color = c("2023" = "darkblue", "2024" = "darkgreen")
  
  data$year = year(data$date)
  data$month = month(data$date)
  
  names(data)[which(names(data) == var_name)] = "var_of_interest" 
  data.dt = as.data.table(data)
  tmp = data.dt[, by=c("year", "month"), .(number_of_NA = length(which(is.na(var_of_interest))),
                                           number_normally_recorded = length(date)
  )]
  tmp$Prop_missing = tmp$number_of_NA / tmp$number_normally_recorded 
  tmp$Month = as.factor(tmp$month)
  tmp$Year = as.factor(tmp$year)
  
  plot2beReturned = ggplot(data = tmp, aes(x = Month, y = Prop_missing, fill = Year)) +
    geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
    scale_x_discrete(breaks=1:12,
                     labels = Month_list) +
    scale_fill_manual(values = Year_color) +
    xlab("") +
    coord_polar(direction = 1) +
    theme_light() +
    ylab(paste0("Proportion of missing data")) + 
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=14, face="bold")) + 
    #ggtitle(var_name) 
     labs(title=var_name,
       subtitle=paste0("Missing data wrt ", tempRes, " records"))
       
  return(plot2beReturned)
}