library(tidyverse)
library(rvest)

url <- 'https://www.portlandmercury.com/sponsored/burgerweek2021'
webpage <- read_html(url)

restaurant <- html_nodes(webpage,'.headline a') %>% html_text2()
burger_name <- html_nodes(webpage, '.blog-body p:nth-child(1)') %>% html_text2()
description <- html_nodes(webpage, '.blog-body p:nth-child(2)') %>% html_text2()
# Sometimes address/hours are the 4th element, sometimes the 5th. Take both and subset.
address_hours <- html_nodes(webpage, 'p:nth-child(5) , p:nth-child(4)') %>% html_text2() %>% str_subset("Address/Hours")

burger_week <- tibble(restaurant = restaurant,
                      burger_name = burger_name,
                      description = description,
                      address_hours = address_hours)
# cleaning
burger_week <- burger_week %>% 
  separate(burger_name, into = c("trash", "burger_name"), sep = ":", extra = "merge") %>% 
  separate(description, into = c("trash", "description"), sep = ":", extra = "merge") %>%
  separate(address_hours, into = c("trash", "address_hours"), sep = ":", extra = "merge") %>%
  select(restaurant, burger_name, description, address_hours)

write_csv(x = burger_week, path = "burger_week_2021.csv")
