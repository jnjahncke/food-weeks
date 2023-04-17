library(tidyverse)
library(rvest)
library(googlesheets4)

url <- 'https://everout.com/portland/events/portland-pizza-week-2023/e141961/'
webpage <- read_html(url)

# get restaurant and pizza names
restaurants <- html_nodes(webpage, 'h4 a') %>% html_text2()
pizzas <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to pizza descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# for building tibble, make sure each element is in the list
is_in_list <- function(label, description) {
  if (description %>% grepl(pattern = label)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# for parsing the restaurant/address/hours
parse_address <- function(ah) {
  split <- ah %>% str_split(", ", n = 2) %>% .[[1]]
  restaurant <- split[1]
  a_h <- split[2]
  
  # determine a_h format
  usual <- a_h %>% str_split(" / ", n = 2) %>% .[[1]]
  b <- a_h %>% str_split(", | / ", n = 3) %>% .[[1]]
  pattern <- "(?<address1>.*) [/(](?<hours1>.*)[/)] & (?<address2>.*) [/(](?<hours2>.*)[/)]"
  c <- a_h %>% str_match_all(pattern) %>% .[[1]]
  if (length(usual) == 2 & str_detect(usual[2], "daily|day") & !str_detect(usual[1], "daily|day")) {
    address <- usual[1]
    hours <- usual[2]
    return(c(restaurant, address, hours))
  } else if (length(b) == 3 & str_detect(b[1], "daily|day") & str_detect(b[3], "daily|day")) {
    address <- b[2]
    hours <- paste0(b[1],", ",b[3])
    return(c(restaurant, address, hours))
  } else {
    address1 <- c %>% .[1,"address1"] %>% as.character()
    address2 <- c %>% .[1,"address2"] %>% as.character()
    hours1 <- c %>% .[1,"hours1"] %>% as.character()
    hours2 <- c %>% .[1,"hours2"] %>% as.character()
    return(c(restaurant, address1, hours1, address2, hours2))
  }
}

# label dictionary - will be used to name columns
label_dict <- c("What It's Called:" = "pizza",
                "What's On It:" = "toppings",
                "What They Say About It:" = "description",
                "Where and When to Get It:" = "address_hours",
                "Meat or Vegetarian?" = "meat_veggie",
                "By the Slice or Whole Pie?" = "slice_pie",
                "Allow Minors?" = "minors",
                "Allow Takeout?" = "takeout",
                "Allow Delivery?" = "delivery",
                "Purchase Limit per Customer?" = "purchase_limit",
                "Daily Availability Limit?" = "available_limit")

# loop through pizza links, get info, build pizza data table
pizza_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  
  labels <- html_nodes(webpage, 'span strong') %>% html_text2() %>% str_squish()
  info <- html_nodes(webpage, '.description') %>% html_text2() %>% .[1]
  
  
  # make sub-list of labels that are in the description
  labels2 <- c()
  label_list <- "What It's Called: "
  for (label in labels) {
    if (is_in_list(label,info)) {
      labels2 <- labels2 %>% append(label)
      if (label == "The Fine Print!") {
        label_list <- paste0(label_list,"|",label)
      } else {
        label_list <- paste0(label_list,"|",label," ")
      }
    }
  }
  
  # split at each label
  info <- info %>% str_split_1(label_list %>% str_replace_all("[?]","[?]")) %>% .[-1]
  
  # build tibble
  temp_tib <- tibble(pizza = info[1])
  for (j in seq(2,length(info))) {
      name <- paste(label_dict[labels2[j]])
      val <- paste0(info[j])
      temp_tib <- temp_tib %>% mutate(!!name := val)
  }
  
  # clean address
  rah <- parse_address(temp_tib$address_hours)
  if (length(rah) == 3) {
    # sometimes there are two addresses in the address field
    # split these into two entries
    if (str_detect(rah[2], " and ")) {
      addresses = str_split(rah[2], " and ") %>% .[[1]]
      temp_tib1 <- temp_tib %>% 
        select(-address_hours) %>% 
        mutate(restaurant = rah[1],
               address = addresses[1],
               hours = rah[3])
      temp_tib2 <- temp_tib %>% 
        select(-address_hours) %>% 
        mutate(restaurant = rah[1],
               address = addresses[2],
               hours = rah[3])
      temp_tib <- temp_tib1 %>% bind_rows(temp_tib2)
    } else {
      temp_tib <- temp_tib %>%
        select(-address_hours) %>% 
        mutate(restaurant = rah[1],
               address = rah[2],
               hours = rah[3]) 
    }
  # in cases where two addresses/two hours were already
  # found by the address parser
  } else {
    temp_tib1 <- temp_tib %>% 
      select(-address_hours) %>% 
      mutate(restaurant = rah[1],
             address = rah[2],
             hours = rah[3])
    temp_tib2 <- temp_tib %>% 
      select(-address_hours) %>% 
      mutate(restaurant = rah[1],
             address = rah[4],
             hours = rah[5])
    temp_tib <- temp_tib1 %>% bind_rows(temp_tib2)
  }
  
  pizza_week <- pizza_week %>% bind_rows(temp_tib)
}

# cleaning
pizza_week <- pizza_week %>% 
  rename(notes = "NA") %>% 
  select(restaurant, pizza, toppings, description, address, hours, everything()) %>% 
  relocate(notes, .after = last_col())


###################### parse hours of availability ######################

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

# export
save(pizza_week_hrs, file = "pizza_week.RData")
write_csv(x = pizza_week_hrs, file = "pizza_week_2023.csv")

# save to google sheets so we can vote:
pizza_week_hrs <- pizza_week_hrs %>% mutate(JJ = NA, RR = NA) %>% select(restaurant, pizza, JJ, RR, everything())
gs4_create("pizza-week-2023", sheets = pizza_week)