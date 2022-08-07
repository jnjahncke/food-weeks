library(tidyverse)
library(rvest)

url <- 'https://everout.com/portland/events/portland-mercurys-burger-week-2022/e123347/'
webpage <- read_html(url)

# get restaurant and burger names
restaurants <- html_nodes(webpage, '.text-center h4') %>% html_text2()
burgers <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to sandwich descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# loop through sandwich links, get info, build sandwich data table
burger_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  info <- html_nodes(webpage, '.fs-5') %>% html_text2()
  
  toppings_temp <- info %>% str_subset("What's on it")
  address_temp <- info %>% str_subset("Address")
  fine_print_temp <- info %>% str_subset("Allow")
  
  temp_tib <- tibble(restaurant = restaurants[i],
                     burger = burgers[i],
                     info = toppings_temp)
  
  burger_week <- burger_week %>% bind_rows(temp_tib)
}

# clean
burger_week <- burger_week %>% 
  separate(info, into = c("toppings", "address_hours", "fine_print"), sep = "Address|The Fine Print!") %>% 
  separate(toppings, into = c("trash", "toppings"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("trash","address_hours"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("address","hours"), sep = "[/]", extra = "merge") %>% 
  select(restaurant, burger, toppings, address, hours, fine_print)

# save
write_csv(x = burger_week, file = "burger_week_2022.csv")
