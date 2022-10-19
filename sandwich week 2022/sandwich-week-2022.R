library(tidyverse)
library(rvest)

url <- 'https://everout.com/portland/events/portland-mercurys-sandwich-week-2022/e118205/'
webpage <- read_html(url)

# get restaurant and sandwich names
restaurant_sandwich <- html_nodes(webpage, '.mb-4') %>% html_text2()
rest_len <- length(restaurant_sandwich)-1
restaurant_sandwich <- restaurant_sandwich[2:rest_len]
restaurant_sandwich <- restaurant_sandwich %>% as.tibble() %>% rename(restaurant = value) %>% 
  separate(restaurant, into = c("restaurant","sandwich"), sep = "\n")

restaurants <- restaurant_sandwich$restaurant %>% unlist()
sandwiches <- restaurant_sandwich$sandwich %>% unlist()

# list of links to sandwich descriptions
sand_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# loop through sandwich links, get info, build sandwich data table
sandwich_week <- tibble()
for (i in 1:length(sand_links)) {
  url <- sand_links[i]
  webpage <- read_html(url)
  info <- html_nodes(webpage, '.mb-5 p') %>% html_text2()
  
  toppings_temp <- info %>% str_subset("What's on it")
  inspiration_temp <- info %>% str_subset("The Inspiration")
  address_temp <- info %>% str_subset("Address")
  fine_print_temp <- info %>% str_subset("Allow")
  
  temp_tib <- tibble(restaurant = restaurants[i],
                     sandwich = sandwiches[i],
                     toppings = toppings_temp,
                     inspiration = inspiration_temp,
                     address_hours = address_temp,
                     fine_print = fine_print_temp)
  
  sandwich_week <- sandwich_week %>% bind_rows(temp_tib)
}

# cleaning
sandwich_week <- sandwich_week %>% 
  separate(toppings, into = c("trash","toppings"), sep = ": ", extra = "merge") %>% 
  separate(inspiration, into = c("trash","inspiration"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("trash","address_hours"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("address","hours"), sep = "[/]|,", extra = "merge") %>% 
  select(restaurant, sandwich, toppings, inspiration, address, hours, fine_print)

# save
write_csv(x = sandwich_week, file = "sandwich_week_2022.csv")
