library(tidyverse)

load("pizza_week.RData")

# usually: restaurant, address / hours1, hours2, etc.
# sometimes:
#   retaurant, hours, address / more hours
#   restaurant, address1 (hours) & address2 (hours)

# hours: daily, am, pm, day

address_hours <- pizza_week$address_hours

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

test <- address_hours[8]
parse_address(test)
