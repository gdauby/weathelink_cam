
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

####
#### Function to transform cardinal direction wind directions to decimal degrees. 
####

require(dplyr)

# Scraped from http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm
cardinal_directions_look_up <- function(minimal = TRUE) {
  
  df <- dplyr::tribble(
    ~cardinal_direction, ~degrees_direction_min, ~degrees_direction_max, ~degrees_direction_mean,
    "N",                 348.75,                 11.25,                  360,                    
    "NNE",               11.25,                  33.75,                  22.5,                   
    "NE",                33.75,                  56.25,                  45,                     
    "ENE",               56.25,                  78.75,                  67.5,                   
    "E",                 78.75,                  101.25,                 90,                     
    "ESE",               101.25,                 123.75,                 112.5,                  
    "SE",                123.75,                 146.25,                 135,                    
    "SSE",               146.25,                 168.75,                 157.5,                  
    "S",                 168.75,                 191.25,                 180,                    
    "SSW",               191.25,                 213.75,                 202.5,                  
    "SW",                213.75,                 236.25,                 225,                    
    "WSW",               236.25,                 258.75,                 247.5,                  
    "W",                 258.75,                 281.25,                 270,                    
    "WNW",               281.25,                 303.75,                 292.5,                  
    "NW",                303.75,                 326.25,                 315,                    
    "NNW",               326.25,                 348.75,                 337.5
  )
  
  # Select variables 
  if (minimal) df <- dplyr::select(df, cardinal_direction, degrees_direction_mean)
  
  return(df)
  
}

cardinal_direction_to_degrees <- function(x, north_is = 360) {
  
  # Check inputs
  stopifnot(north_is %in% c(360, 0))
  
  # Make sure vector is uppercase
  x <- stringr::str_to_upper(x)
  
  # Change northerly degrees to zero
  if (north_is == 0) {
    df_look_up <- cardinal_directions_look_up() %>% 
      mutate(degrees_direction_mean = if_else(cardinal_direction == "N", 0, degrees_direction_mean))
  } else {
    df_look_up <- cardinal_directions_look_up()
  }
  
  # Do the transformation with a join
  x <- dplyr::tibble(cardinal_direction = x) %>% 
    left_join(df_look_up, by = "cardinal_direction") %>% 
    pull(degrees_direction_mean)
  
  return(x)
  
}