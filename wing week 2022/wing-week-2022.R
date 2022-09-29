library(tidyverse)
library(rvest)

url <- 'https://everout.com/portland/events/portland-wing-week-2022/e126188/'
webpage <- read_html(url)

# get restaurant and burger names
restaurants <- html_nodes(webpage, 'h4') %>% html_text2()
wings <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to sandwich descriptions
wing_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# loop through wing links, get info, build wing data table
wing_week <- tibble()

for (i in 1:length(wing_links)) {
  url <- wing_links[i]
  webpage <- read_html(url)
  info <- html_nodes(webpage, '.fs-5') %>% html_text2()
  
  
  temp_tib <- tibble(restaurant = restaurants[i],
                     wing = wings[i],
                     info = info)
  
  wing_week <- wing_week %>% bind_rows(temp_tib)
}

wing_week <- wing_week %>% filter(grepl("What", info))

# clean
wing_week <- wing_week %>%
  separate(info, into = c("toppings", "address_hours", "fine_print"), sep = "Address|The Fine Print!") %>% 
  separate(toppings, into = c("trash", "toppings"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("trash","address_hours"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("address","hours"), sep = "[/]", extra = "merge") %>% 
    separate(fine_print, 
           into = c("trash", "minors","takeout","delivery","purchase_limit","gluten","vegan_available"),
           sep = "[?]",
           extra = "merge") %>% 
  separate(minors, into = c("minors","trash"), sep = "Allow") %>% 
  separate(takeout, into = c("takeout","trash"), sep = "Allow") %>% 
  separate(delivery, into = c("delivery","trash"), sep = "Purchase") %>% 
  separate(purchase_limit, into = c("purchase_limit","trash"), sep = "Available") %>% 
  separate(gluten, into = c("gluten","trash"), sep = "Available") %>% 
  select(restaurant, wing, toppings, address, hours, everything()) %>% 
  select(-trash)

# save
write_csv(x = wing_week, file = "wing_week_2022.csv")
