library(tidyverse)

pizza_week <- load("pizza_week.RData")

hours <- pizza_week$hours

# parse_times: takes in a time range and returns the start and stop times in xx:xx am/pm-xx:xx am/pm format
parse_times <- function(trange) {
  trange <- str_replace(trange,"midnight","11:59 pm")
  begin_end <- trange %>% str_split(pattern = "–|-")
  begin <- begin_end[[1]][1]
  end <- begin_end[[1]][2]
  begin_ampm <- c(str_detect(begin, pattern = "am"), str_detect(begin, pattern = "pm"))
  end_ampm <- c(str_detect(end, pattern = "am"), str_detect(end, pattern = "pm"))
  # TRUE FALSE = am
  # FALSE TRUE = pm
  # FALSE FALSE for begin, means they are both whatever end is
  if (end_ampm[1] == TRUE & end_ampm[2] == FALSE) {
    end_suffix = "am"
    end_time <- end %>% str_split("am") %>% .[[1]] %>% .[1] %>% str_squish()
  } else if (end_ampm[1] == FALSE & end_ampm[2] == TRUE) {
    end_suffix = "pm"
    end_time <- end %>% str_split("pm") %>% .[[1]] %>% .[1] %>% str_squish()}
  if (begin_ampm[1] == TRUE & begin_ampm[2] == FALSE) {
    begin_suffix = "am"
    begin_time <- begin %>% str_split("am") %>% .[[1]] %>% .[1] %>% str_squish()
    } else if (begin_ampm[1] == FALSE & begin_ampm[2] == TRUE) {
      begin_suffix = "pm"
      begin_time <- begin %>% str_split("pm") %>% .[[1]] %>% .[1] %>% str_squish()} else {
        begin_suffix = end_suffix
        begin_time <- begin %>% str_squish()}
  
  if (!str_detect(begin_time, ":")) {begin_time <- paste0(begin_time,":00")}
  if (!str_detect(end_time, ":")) {end_time <- paste0(end_time,":00")}
  
  begin <- paste0(begin_time, " ", begin_suffix)
  end <- paste0(end_time, " ", end_suffix)
  
  return(paste0(begin,"-",end))
}

# parse_days: takes in a day range and returns a list containing all of the days in that range
parse_days <- function(drange) {
  
  if (str_detect(drange,"–|-")) {
    drange <- drange %>% str_split("–|-")
    
    days = c.factor("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") %>% as_factor() %>% ordered()
    a <-  which(days == drange[[1]][1])
    b <- which(days == drange[[1]][2])
    schedule <- days[a:b] %>% as.character()
    
    return(schedule)} else {return(drange %>% str_squish())}
}

# parse_hours takes in days and times and returns a full schedule
parse_hours <- function(hrs) {
  hrs <- hrs %>% str_remove(pattern = " [(]we are not offering it on Friday or Saturday night dinner service, it's just too busy![)]")
  
  if (str_detect(hrs, "daily")) {
    daily <- hrs %>% str_split(" daily") %>% .[[1]] %>% .[1] %>% parse_times()
    schedule <- c("Monday" = daily,
                  "Tuesday" = daily,
                  "Wednesday" = daily,
                  "Thursday" = daily,
                  "Friday" = daily,
                  "Saturday" = daily,
                  "Sunday" = daily)
    return(schedule %>% enframe())
  } else {
    hrs_split <- hrs %>% str_split(",") %>% .[[1]]
    
    if (!str_detect(hrs_split[1],"[0-9]")) {hrs_split <- hrs %>% str_remove(",")}
    
    schedule = c()
    for (i in seq(1,length(hrs_split))) {
      if (!str_detect(hrs_split[i],"Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday")) {
        last <- hrs_split[i-1] %>% str_squish() %>% str_split(" ", n = 2) %>% .[[1]]
        drange <- last[1] %>% str_squish()
        trange <- hrs_split[i] %>% str_squish()
      } else {
        h_temp <- hrs_split[i] %>% str_squish() %>% str_split(" ", n = 2) %>% .[[1]]
        drange <- h_temp[1] %>% str_squish()
        trange <- h_temp[2] %>% str_squish()
      }
      days <- parse_days(drange)
      times <- parse_times(trange)
      for (day in days) {
        schedule[day] = times
      }
    }
    
    return(schedule %>% enframe()) # return as a tibble
  }
}

pizza_week_hrs <- tibble()
for (a in pizza_week$address) {
  temp <- pizza_week %>% filter(address == a)
  schedule <- parse_hours(temp$hours) %>% pivot_wider(names_from = name, values_from = value)
  temp <- bind_cols(temp, schedule)
  pizza_week_hrs <- bind_rows(pizza_week_hrs, temp)
}
