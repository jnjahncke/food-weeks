library(tidyverse)
library(rvest)

url <- 'https://www.portlandmercury.com/sponsored/wingweek2021'
webpage <- read_html(url)

restaurant <- html_nodes(webpage,'.headline a') %>% html_text2()
wing_name <- html_nodes(webpage, '.blog-body p:nth-child(1)') %>% html_text2()
description <- html_nodes(webpage, '.blog-body p:nth-child(2)') %>% html_text2()
address_hours <- html_nodes(webpage, 'p:nth-child(4)') %>% html_text2() %>% str_subset("Address/Hours")

wing_week <- tibble(restaurant = restaurant,
                    wing_name = wing_name,
                    description = description,
                    address_hours = address_hours)
# cleaning
wing_week <- wing_week %>% 
  separate(wing_name, into = c("trash", "wing_name"), sep = ":", extra = "merge") %>% 
  separate(description, into = c("trash", "description"), sep = ":", extra = "merge") %>%
  separate(address_hours, into = c("trash", "address_hours"), sep = ":", extra = "merge") %>%
  select(restaurant, wing_name, description, address_hours)

write_csv(x = wing_week, path = "wing_week_2021.csv")
