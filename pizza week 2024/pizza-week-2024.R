library(tidyverse)
library(rvest)
library(tidygeocoder)
library(sf)
library(googlesheets4)

url <- 'https://everout.com/portland/events/the-portland-mercurys-pizza-week-2024/e170026/'
webpage <- read_html(url)

# get restaurant and wing names
restaurants <- html_nodes(webpage, 'h4 a') %>% html_text2()
pizzas <- html_nodes(webpage, 'h3 a') %>% html_text2()
img_links <- html_elements(webpage, '.img-fluid') %>% html_attr("src")

# list of links to wing descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# label dictionary - will be used to name columns
label_dict <- c("What It's Called:" = "pizza",
                "What's On It:" = "toppings",                 
                "What They Say About It:" = "description",
                "Where and When to Get It:" = "address_hours",      
                "The Fine Print!" = "fine_print",
                "Meat or Vegetarian?" = "meat_veggie",
                "Vegetarian Substitute?" = "veggie_sub",
                "Vegan Substitute?" = "vegan_sub",
                "Gluten Free?" = "gf",
                "Gluten Free Substitute?" = "gf_sub",
                "Whole Pie or Slice?" = "whole_slice",
                "Allow Minors?" = "minors",
                "Allow Takeout?" = "takeout",
                "Allow Delivery?" = "delivery",
                "Purchase Limit per Customer?" = "purchase_limit",
                "Daily Availability Limit?" = "availability_limit")

# loop through pizza links, get info, build pizza data table
pizza_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  
  
  info <- html_nodes(webpage, '.description') %>% html_text2() %>% .[1]
  labels <- str_match_all(string = info, pattern = "(What It's Called:|What It's Called:|What's On It:|What They Say About It:|Where and When to Get It:|The Fine Print!|Meat or Vegetarian\\?|Vegetarian Substitute\\?|Vegan Substitute\\?|Gluten Free\\?|Gluten Free Substitute\\?|Whole Pie or Slice\\?|Allow Minors\\?|Allow Takeout\\?|Allow Delivery\\?|Purchase Limit per Customer\\?|Daily Availability Limit\\?)")[[1]][,2]
  
  
  # split at each label
  label_list = c()
  for (label in labels){
    label <- str_replace(label,"[?]","\\\\?")
    label_list <- label_list %>% append(label)
  }
  label_list <- paste(unlist(label_list), collapse='|')
  
  info <- info %>% str_split_1(label_list) %>% .[-1]
  
  info2 <- c()
  for (element in info) {
    element <- str_squish(element)
    info2 <- append(info2, str_replace(element, "^ |^[?] ", ""))
  }
  
  # build tibble
  temp_tib <- tibble(pizza = info2[1])
  for (j in seq(2,length(info2))) {
    name <- paste(label_dict[paste0(labels[j][[1]])]) 
    val <- paste0(info2[j])
    temp_tib <- temp_tib %>% mutate(!!name := val) %>% mutate(restaurant = restaurants[i], image = img_links[i])
  }

  if (dim(pizza_week)[1] == 0) {
    pizza_week <- pizza_week %>% bind_rows(temp_tib)
  } else {
    pizza_week <- pizza_week %>% full_join(temp_tib)
    }
  
  rm(info, info2, url, webpage, labels, label_list, temp_tib, name, val, element, i, j)
}

##### cleaning #####

# remove restaurant name from start of address_hours
names <- restaurants %>% append(c("Von Ebert Beaverton", "Von Ebert Cascade Station", "Von Ebert Pearl", "Von Ebert Glendoveer"))
pizza_week2 <- tibble()
for (i in seq(1,nrow(pizza_week))) {
  temp <- pizza_week[i,]
  ah <- temp$address_hours %>% str_split(", |: ", n = 2)
  if (ah[[1]][1] %in% names) {
    temp$address_hours <- ah[[1]][2]
  }
  pizza_week2 <- pizza_week2 %>% bind_rows(temp)
}
pizza_week <- pizza_week2 %>% select(-fine_print)
rm(pizza_week2, names, temp, ah, i)



# duplicate entries if there are two addresses associated
locations <- read_csv("locations.csv")
pizza_week <- full_join(pizza_week, locations)
pizza_week2 <- tibble()
for (i in seq(1,nrow(pizza_week))) {
  temp <- pizza_week[i,]
  if (temp$locations > 1) {
    loc_sep <- temp$loc_sep
    loc_split <- str_split(loc_sep, ", ", simplify = T)
    if (length(loc_split) > 1) {
      ah <- temp$address_hours %>% 
        str_split(loc_split[1], simplify = T) %>% .[2] %>% 
        str_split(loc_split[2], simplify = T)
      temp1 <- temp %>% mutate(address_hours = ah[1] %>% str_squish())
      temp2 <- temp %>% mutate(address_hours = ah[2] %>% str_squish())
      pizza_week2 <- pizza_week2 %>% bind_rows(temp1, temp2)
    } else {
      ah <- temp$address_hours %>% str_split(loc_sep, simplify = T)
      temp1 <- temp %>% mutate(address_hours = ah[1] %>% str_squish())
      temp2 <- temp %>% mutate(address_hours = ah[2] %>% str_squish())
      pizza_week2 <- pizza_week2 %>% bind_rows(temp1, temp2)
      }}
  else {
    pizza_week2 <- pizza_week2 %>% bind_rows(temp)
  }
}
pizza_week <- pizza_week2 %>% select(-locations, -loc_sep)
rm(temp, temp1, temp2, pizza_week2, loc_split, loc_sep, ah, i)

# separate address_hours into address and hours
# REGEX:
### Group 1 = Everything
### Group 2 = Address
### Group 3 = Hours
pattern <- "((?:\\d{1,5}) (?:[NSEWest]{1,4}\\.?) (?:[A-z]|\\d|\\s|#)+\\,?\\.? ?(?:Suite \\d{1,5})?(?:Beaverton, OR)?) ?(?:\\/|-|\\.)? ?(.+)"
pizza_week <- pizza_week %>% mutate(address = str_match(string = address_hours, pattern = pattern)[,2],
                      hours = str_match(string = address_hours, pattern = pattern)[,3]) %>% 
  select(-address_hours)

# add city to address
for (addy in pizza_week$address) {
  if (str_detect(addy, "Portland|Lake Oswego|Tigard|Beaverton|Hillsboro|Gresham") == FALSE) {
    temp_tib <- pizza_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", Portland, OR"))
  } else if (str_detect(addy, "OR$|\\d$") == FALSE) {
    temp_tib <- pizza_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", OR"))
  } else {
    temp_tib <- pizza_week %>% filter(address == addy)
  }
  pizza_week <- pizza_week %>% filter(address != addy) %>% 
    bind_rows(temp_tib) %>% 
    arrange(restaurant)
  rm(temp_tib)
}

# arrange columns
pizza_week <- pizza_week %>% select(pizza, restaurant, toppings, description, image, address, hours, everything())

# export
pizza_week %>% select(-image) %>% save(file = "pizza_week.RData")
pizza_week %>% select(-image) %>% write_csv(file = "pizza_week_2024.csv")

# save to google sheets so we can vote:
# format image url for google sheets
formula <- '=IMAGE("'
end <- '")'
pizza_week_g <- pizza_week %>% mutate(image = paste0(formula, image, end))
gs4_create("pizza-week-2024", sheets = pizza_week_g)


### for shiny app ###
# look up gps coordinates
pizza_week <- pizza_week %>% geocode(address, method = 'arcgis', lat = latitude , long = longitude)

# convert coordinates to mercator
geometry <- as_tibble(sf_project(st_crs(4326), st_crs(3857), pizza_week %>% select(longitude, latitude)))
pizza_week <- pizza_week %>% bind_cols(geometry) %>% 
  rename(lon = V1, lat = V2)

# load pdx gis data
load("pdx_gis.RData")
save(pizza_week, pdx, river_boundaries, file = "for_shiny.RData")
