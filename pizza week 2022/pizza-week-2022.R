library(tidyverse)
library(rvest)

url <- 'https://www.portlandmercury.com/sponsored/pizzaweek2022'
webpage <- read_html(url)

restaurant <- html_nodes(webpage, '.headline a') %>% html_text2()
pizza_name <- html_nodes(webpage,'.blog-body p:nth-child(1)') %>% html_text2() %>% str_replace_all("[\n]" , " ") %>% str_replace_all("Pizza Name: ", "")
content <- html_nodes(webpage, '.blog-body p+ p') %>% html_text2()

pizza_week <- tibble(restaurant = restaurant,
                     pizza = pizza_name,
                     toppings = content %>% str_subset("What's on it"),
                     address_hours = content %>% str_subset("Address"),
                     details = content %>% str_subset("Allow"))
# cleaning
pizza_week <- pizza_week %>% 
  separate(toppings, into = c("trash", "toppings"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("trash", "address_hours"), sep = ": ", extra = "merge") %>% 
  separate(address_hours, into = c("address", "hours"), sep = " / ", extra = "merge") %>% 
  select(restaurant, pizza, toppings, address, hours, details)

write_csv(x = pizza_week, path = "pizza_week_2022.csv")
