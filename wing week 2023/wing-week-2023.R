library(tidyverse)
library(rvest)
library(tidygeocoder)
library(sf)
library(googlesheets4)

url <- 'https://everout.com/portland/events/portland-mercurys-wing-week-2023/e156565/'
webpage <- read_html(url)

# get restaurant and wing names
restaurants <- html_nodes(webpage, 'h4 a') %>% html_text2()
wings <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to wing descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# label dictionary - will be used to name columns
label_dict <- c("What They're Called:" = "wing",
                "What's On Them:" = "toppings",                 
                "What They Say About Them:" = "description",
                "Where and When To Get Them:" = "address_hours",      
                "The Fine Print!" = "fine_print",
                "Allow Minors?" = "minors",               
                "Allow Takeout?" = "takeout",
                "Allow Delivery?" = "delivery",                   
                "Purchase Limit?" = "purchase_limit",
                "Chicken or Vegetarian?" = "chick_veggie",           
                "Available Gluten-Free?" = "gluten_free",
                "Available Vegetarian/Vegan?" = "veggie")

# loop through wing links, get info, build wing data table
wing_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  
  labels <- html_nodes(webpage, 'br+ strong , .mb-5 strong:nth-child(1)') %>% html_text2() %>% str_squish()
  info <- html_nodes(webpage, '.description') %>% html_text2() %>% .[1]
  
  
  
  # split at each label
  label_list = "What They're Called:"
  for (label in labels){
    label_list <- paste0(label_list, "|", label)
  }
  
  info <- info %>% str_split_1(label_list) %>% .[-1]
  
  info2 <- c()
  for (element in info) {
    info2 <- append(info2, str_replace(element, "^ |^[?] ", ""))
  }
  
  # build tibble
  temp_tib <- tibble(wing = info2[1])
  for (j in seq(2,length(info2))) {
    # if (str_detect(labels[j],"Gluten") == TRUE) {
    #   name <- "gluten_free"
    # } else {
    #   name <- paste(label_dict[paste0(labels[j][[1]])]) 
    # }
    name <- paste(label_dict[paste0(labels[j][[1]])]) 
    val <- paste0(info2[j])
    temp_tib <- temp_tib %>% mutate(!!name := val) %>% mutate(restaurant = restaurants[i])
  }

  if (dim(wing_week)[1] == 0) {
    wing_week <- wing_week %>% bind_rows(temp_tib)
  } else {
    wing_week <- wing_week %>% full_join(temp_tib)
    }
  
  rm(info, info2, url, webpage, labels, label_list, temp_tib, name, val)
}

# cleaning
wing_week <- wing_week %>% 
  separate(address_hours, into = c("restaurant", "address_hours"), sep = ", ", remove = TRUE, extra = "merge") %>% 
  separate(address_hours, into = c("address","hours"), sep = " [/] ", remove = TRUE, extra = "merge") %>% 
  select(restaurant, wing, toppings, description, address, hours, everything()) %>% 
  select(-fine_print, `NA`)

# Top Burmese Bistro Royale
temp_tib1 <- wing_week %>% filter(restaurant == "Top Burmese Ambassador") %>% 
  separate(hours, into = c("hours", "etc"), sep = " and ") %>% select(-etc)
temp_tib2 <- wing_week %>% filter(restaurant == "Top Burmese Ambassador") %>% 
  separate(hours, into = c("etc", "hours"), sep = " and ") %>% select(-etc, -restaurant, -address) %>% 
  separate(hours, into = c("restaurant", "address_hours"), sep = ", ", remove = TRUE, extra = "merge") %>% 
  separate(address_hours, into = c("address", "hours"), sep = " [/] ")
wing_week <- wing_week %>% filter(restaurant != "Top Burmese Ambassador") %>% 
  full_join(temp_tib1) %>% full_join(temp_tib2) %>% 
  arrange(restaurant)


# duplicate entries if there are two addresses associated
for (w in wing_week$wing) {
  temp_tib <- wing_week %>% filter(wing == w)
  if (str_detect(temp_tib$address[[1]]," & |; | and ")) {
    temp_tib1 <- temp_tib %>% select(-address) %>% mutate(address = temp_tib$address[[1]] %>% str_split_1(" & |; | and ") %>% .[1])
    temp_tib2 <- temp_tib %>% select(-address) %>% mutate(address = temp_tib$address[[1]] %>% str_split_1(" & |; | and ") %>% .[2])
    wing_week <- wing_week %>% filter(wing != w) %>% 
      bind_rows(temp_tib1) %>% 
      bind_rows(temp_tib2)
    rm(temp_tib1, temp_tib2)
  }
  rm(temp_tib)
}

# add city to address
for (addy in wing_week$address) {
  if (str_detect(addy, "Portland|Lake Oswego|Tigard|Beaverton|Hillsboro|Gresham") == FALSE) {
    temp_tib <- wing_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", Portland, OR"))
  } else if (str_detect(addy, "OR$|\\d$") == FALSE) {
    temp_tib <- wing_week %>% filter(address == addy) %>% select(-address) %>% 
      mutate(address = paste0(addy, ", OR"))
  } else {
    temp_tib <- wing_week %>% filter(address == addy)
  }
  wing_week <- wing_week %>% filter(address != addy) %>% 
    bind_rows(temp_tib) %>% 
    arrange(restaurant)
  rm(temp_tib)
}

# export
save(wing_week, file = "wing_week.RData")
write_csv(x = wing_week, file = "wing_week_2023.csv")

# save to google sheets so we can vote:
gs4_create("wing-week-2023", sheets = wing_week)


### for shiny app ###
# look up gps coordinates
wing_week <- wing_week %>% geocode(address, method = 'arcgis', lat = latitude , long = longitude)

# convert coordinates to mercator
geometry <- as_tibble(sf_project(st_crs(4326), st_crs(3857), wing_week %>% select(longitude, latitude)))
wing_week <- wing_week %>% bind_cols(geometry) %>% 
  rename(lon = V1, lat = V2)

# load pdx gis data
load("pdx_gis.RData")
save(wing_week, pdx, river_boundaries, file = "for_shiny.RData")
