library(tidyverse)
library(rvest)
library(tidygeocoder)
library(sf)
library(googlesheets4)

url <- 'https://everout.com/portland/events/portland-mercurys-sandwich-week-2023/e145067/'
webpage <- read_html(url)

# get restaurant and sandwich names
restaurants <- html_nodes(webpage, '.text-center h4') %>% html_text2()
sandwiches <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to sandwich descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# label dictionary - will be used to name columns
label_dict <- c("What It's Called:" = "sandwich",
                "What's On It:" = "toppings",
                "What They Say About It:" = "description",
                "Where and When to Get It:" = "address_hours",
                "The Fine Print!" = "fine_print",
                "Meat or Vegetarian?" = "meat_veggie",
                "Allow Minors?" = "minors",
                "Allow Takeout?" = "takeout",
                "Allow Delivery?" = "delivery",
                "Purchase Limit per Customer?" = "purchase_limit",
                "Daily Availability Limit?" = "available_limit",
                "More Details:" = "notes")

# loop through sandwich links, get info, build sandwich data table
sandwich_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  
  labels <- html_nodes(webpage, '.mb-5 strong') %>% html_text2() %>% str_squish()
  info <- html_nodes(webpage, '.description') %>% html_text2() %>% .[1]
  
  
  
  # split at each label
  label_list = "What It's Called:"
  for (label in labels){
    label_list <- paste0(label_list, "|", label)
  }
  
  info <- info %>% str_split_1(label_list) %>% .[-1]
  
  info2 <- c()
  for (element in info) {
    info2 <- append(info2, str_replace(element, "^ |^[?] ", ""))
  }
  
  # build tibble
  temp_tib <- tibble(sandwich = info2[1])
  for (j in seq(2,length(info2))) {
    name <- paste(label_dict[j])
    val <- paste0(info2[j])
    temp_tib <- temp_tib %>% mutate(!!name := val)
  }
  
  sandwich_week <- sandwich_week %>% bind_rows(temp_tib)
  rm(info, info2, url, webpage, labels, label_list, temp_tib, name, val)
}

# cleaning
sandwich_week <- sandwich_week %>% 
  separate(address_hours, into = c("restaurant","address_hours"), sep = ", ", remove = TRUE, extra = "merge") %>% 
  separate(address_hours, into = c("address","hours"), sep = " [/] ", remove = TRUE, extra = "merge") %>% 
  select(restaurant, sandwich, toppings, description, address, hours, everything()) %>% 
  select(-fine_print) %>% 
  relocate(notes, .after = last_col())

# duplicate entries if there are two addresses associated
for (sando in sandwich_week$sandwich) {
  temp_tib <- sandwich_week %>% filter(sandwich == sando)
  if (str_detect(temp_tib$address[[1]]," & ")) {
    temp_tib1 <- temp_tib %>% select(-address) %>% mutate(address = temp_tib$address[[1]] %>% str_split_1(" & ") %>% .[1])
    temp_tib2 <- temp_tib %>% select(-address) %>% mutate(address = temp_tib$address[[1]] %>% str_split_1(" & ") %>% .[2])
    sandwich_week <- sandwich_week %>% filter(sandwich != sando) %>% 
      bind_rows(temp_tib1) %>% 
      bind_rows(temp_tib2)
    rm(temp_tib1, temp_tib2)
  }
  rm(temp_tib)
}

# add city to address
for (addy in sandwich_week$address) {
  if (str_detect(addy, "Lake Oswego|Tigard|Beaverton") == FALSE) {
    temp_tib <- sandwich_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", Portland, OR"))
  } else {
    temp_tib <- sandwich_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", OR"))
  }
  sandwich_week <- sandwich_week %>% filter(address != addy) %>% 
    bind_rows(temp_tib) %>% 
    arrange(restaurant)
  rm(temp_tib)
}

# export
save(sandwich_week, file = "sandwich_week.RData")
write_csv(x = sandwich_week, file = "sandwich_week_2023.csv")

# save to google sheets so we can vote:
gs4_create("sandwich-week-2023", sheets = sandwich_week)


### for shiny app ###
# look up gps coordinates
sandwich_week <- sandwich_week %>% geocode(address, method = 'arcgis', lat = latitude , long = longitude)

# convert coordinates to mercator
geometry <- as_tibble(sf_project(st_crs(4326), st_crs(3857), sandwich_week %>% select(longitude, latitude)))
sandwich_week <- sandwich_week %>% bind_cols(geometry) %>% 
  rename(lon = V1, lat = V2)

# load pdx gis data
load("pdx_gis.RData")

save(sandwich_week, pdx, river_boundaries, file = "for_shiny.RData")
