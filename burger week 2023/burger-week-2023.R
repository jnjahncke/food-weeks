library(tidyverse)
library(rvest)
library(tidygeocoder)
library(sf)
library(googlesheets4)

url <- 'https://everout.com/portland/events/portland-mercurys-burger-week-2023/e142182/'
webpage <- read_html(url)

# get restaurant and burger names
restaurants <- html_nodes(webpage, '.text-center h4') %>% html_text2()
burgers <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to burger descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# label dictionary - will be used to name columns
label_dict <- c("What It's Called:" = "burger",
                "What's On It:" = "toppings",                 
                "What They Say About It:" = "description",
                "Where and When To Get It:" = "address_hours",      
                "Jim Beam Drink Special:" = "JB_special",
                "Jim Beam Shot and Laurelwood Beer Drink Special:" = "JBL_special",
                "The Fine Print!" = "fine_print",
                "Allow Minors?" = "minors",               
                "Allow Takeout?" = "takeout",
                "Allow Delivery?" = "delivery",                   
                "Purchase Limit?" = "purchase_limit",
                "Meat or Vegetarian?" = "meat_veggie",           
                "Available Gluten-Free?" = "gluten_free",
                "Available Vegetarian?" = "veggie",       
                "Available Vegan?" = "vegan")

# loop through burger links, get info, build burger data table
burger_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  
  labels <- html_nodes(webpage, '.mb-5 strong') %>% html_text2() %>% str_squish()
  info <- html_nodes(webpage, '.description') %>% html_text2() %>% .[1]
  
  
  
  # split at each label
  label_list = "What It's Called:"
  for (label in labels){
    if (str_detect(label,"Laurelwood")) {
      label <- str_replace(label, "\\+", "and")
    }
    label_list <- paste0(label_list, "|", label)
  }
  
  info <- info %>% str_replace_all("\\+", "and") %>% str_split_1(label_list) %>% .[-1]
  
  info2 <- c()
  for (element in info) {
    info2 <- append(info2, str_replace(element, "^ |^[?] ", ""))
  }
  
  # build tibble
  temp_tib <- tibble(burger = info2[1])
  for (j in seq(2,length(info2))) {
    if (str_detect(labels[j],"Gluten") == TRUE) {
      name <- "gluten_free"
    } else if (str_detect(labels[j],"Laurelwood") == TRUE) {
      name <- "JBL_special"
    } else {
      name <- paste(label_dict[paste0(str_replace(labels[j][[1]], "\\+", "and"))]) 
    }
    val <- paste0(info2[j])
    temp_tib <- temp_tib %>% mutate(!!name := val) %>% mutate(restaurant = restaurants[i])
  }

  if (dim(burger_week)[1] == 0) {
    burger_week <- burger_week %>% bind_rows(temp_tib)
  } else {
    burger_week <- burger_week %>% full_join(temp_tib)
    }
  
  rm(info, info2, url, webpage, labels, label_list, temp_tib, name, val)
}

# cleaning
burger_week <- burger_week %>% 
  separate(address_hours, into = c("address","hours"), sep = " [/] ", remove = TRUE, extra = "merge") %>% 
  select(restaurant, burger, toppings, description, address, hours, everything()) %>% 
  select(-fine_print, `NA`)

# duplicate entries if there are two addresses associated
for (bg in burger_week$burger) {
  temp_tib <- burger_week %>% filter(burger == bg)
  if (str_detect(temp_tib$address[[1]]," & |; | and ")) {
    temp_tib1 <- temp_tib %>% select(-address) %>% mutate(address = temp_tib$address[[1]] %>% str_split_1(" & |; | and ") %>% .[1])
    temp_tib2 <- temp_tib %>% select(-address) %>% mutate(address = temp_tib$address[[1]] %>% str_split_1(" & |; | and ") %>% .[2])
    burger_week <- burger_week %>% filter(burger != bg) %>% 
      bind_rows(temp_tib1) %>% 
      bind_rows(temp_tib2)
    rm(temp_tib1, temp_tib2)
  }
  rm(temp_tib)
}

# add city to address
for (addy in burger_week$address) {
  if (str_detect(addy, "Lake Oswego|Tigard|Beaverton|Vancouver|Hillsboro") == FALSE) {
    temp_tib <- burger_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", Portland, OR"))
  } else if (str_detect(addy, "OR$|WA$") == FALSE) {
    temp_tib <- burger_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", OR"))
  } else {
    temp_tib <- burger_week %>% filter(address == addy)
  }
  burger_week <- burger_week %>% filter(address != addy) %>% 
    bind_rows(temp_tib) %>% 
    arrange(restaurant)
  rm(temp_tib)
}

# export
save(burger_week, file = "burger_week.RData")
write_csv(x = burger_week, file = "burger_week_2023.csv")

# save to google sheets so we can vote:
gs4_create("burger-week-2023", sheets = burger_week)


### for shiny app ###
# look up gps coordinates
burger_week <- burger_week %>% geocode(address, method = 'arcgis', lat = latitude , long = longitude)

# convert coordinates to mercator
geometry <- as_tibble(sf_project(st_crs(4326), st_crs(3857), burger_week %>% select(longitude, latitude)))
burger_week <- burger_week %>% bind_cols(geometry) %>% 
  rename(lon = V1, lat = V2)

# load pdx gis data
load("pdx_gis.RData")
save(burger_week, pdx, river_boundaries, file = "for_shiny.RData")
