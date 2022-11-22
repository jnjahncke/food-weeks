library(tidyverse)
library(rvest)

url <- 'https://everout.com/portland/events/portland-mercurys-burger-week-2022/e123347/'
webpage <- read_html(url)

# get restaurant and burger names
restaurants <- html_nodes(webpage, '.text-center h4') %>% html_text2()
burgers <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to burger descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# loop through burger links, get info, build burger data table
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
  separate(fine_print, 
           into = c("trash", "minors","takeout","delivery","purchase_limit","meat_veggie","gluten","veggie_available","vegan_available"),
           sep = "[?]",
           extra = "merge") %>% 
  separate(minors, into = c("minors","trash"), sep = "Allow") %>% 
  separate(takeout, into = c("takeout","trash"), sep = "Allow") %>% 
  separate(delivery, into = c("delivery","trash"), sep = "Purchase") %>% 
  separate(purchase_limit, into = c("purchase_limit","trash"), sep = "Meat") %>% 
  separate(meat_veggie, into = c("meat_veggie","trash"), sep = "Available") %>% 
  separate(gluten, into = c("gluten","trash"), sep = "Available") %>% 
  separate(veggie_available, into = c("veggie_available","trash"), sep = "Available") %>% 
  select(restaurant, burger, toppings, address, hours, minors, takeout, delivery, purchase_limit, meat_veggie, gluten, veggie_available, vegan_available)

# save
write_csv(x = burger_week, file = "burger_week_2022.csv")
