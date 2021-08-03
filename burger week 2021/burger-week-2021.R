library(tidyverse)
library(rvest)

url <- 'https://www.portlandmercury.com/sponsored/burgerweek2021?mc_cid=ae65e1e82c&mc_eid=2aaf2541a5'
webpage <- read_html(url)

restaurant <- html_nodes(webpage,'.headline a') %>% html_text()
burger_name <- html_nodes(webpage, '.blog-body p:nth-child(1)') %>% html_text()
description <- html_nodes(webpage, '.blog-body p:nth-child(2)') %>% html_text()
address_hours <- html_nodes(webpage, '.fish-butter:nth-child(59) p:nth-child(4) , .fish-butter:nth-child(57) p:nth-child(5) , .fish-butter:nth-child(55) p:nth-child(5) , .fish-butter:nth-child(53) p:nth-child(4) , .fish-butter:nth-child(51) p:nth-child(5) , .fish-butter:nth-child(49) p:nth-child(4) , .fish-butter:nth-child(47) p:nth-child(5) , .fish-butter:nth-child(45) p:nth-child(4) , .fish-butter:nth-child(43) p:nth-child(5) , .fish-butter:nth-child(41) p:nth-child(5) , .fish-butter:nth-child(39) p:nth-child(5) , .fish-butter:nth-child(37) p:nth-child(5) , .fish-butter:nth-child(35) p:nth-child(5) , .fish-butter:nth-child(33) p:nth-child(4) , .fish-butter:nth-child(31) p:nth-child(5) , .fish-butter:nth-child(29) p:nth-child(5) , .fish-butter:nth-child(27) p:nth-child(5) , .fish-butter:nth-child(25) p:nth-child(5) , .fish-butter:nth-child(23) p:nth-child(5) , .fish-butter:nth-child(21) p:nth-child(5) , .fish-butter:nth-child(19) p:nth-child(4) , .fish-butter:nth-child(17) p:nth-child(4) , .fish-butter:nth-child(15) p:nth-child(4) , .fish-butter:nth-child(13) p:nth-child(4) , .fish-butter:nth-child(11) p:nth-child(5) , .fish-butter:nth-child(9) p:nth-child(4) , .fish-butter:nth-child(7) p:nth-child(4) , .fish-butter:nth-child(5) p:nth-child(4) , .fish-butter:nth-child(3) p:nth-child(5) , .fish-butter:nth-child(1) p:nth-child(4)') %>% html_text()
# lol

burger_week <- tibble(restaurant = restaurant,
                      burger_name = burger_name,
                      description = description,
                      address_hours = address_hours)
# cleaning
burger_week <- burger_week %>% 
  separate(burger_name, into = c("trash", 'burger_name'), sep = ":", extra = "merge") %>% 
  separate(burger_name, into = c("burger_name", "trash"), sep = "\\n|\t") %>% 
  separate(description, into = c("trash", "description"), sep = ":", extra = "merge") %>% 
  separate(description, into = c("description", "trash"), sep = "\\n|\t") %>% 
  separate(address_hours, into = c("trash", "address_hours"), sep = ":", extra = "merge") %>% 
  separate(address_hours, into = c("address_hours", "trash"), sep = "\\n|\t") %>% 
  select(-trash)

write_csv(x = burger_week, path = "burger_week_2021.csv")
