library(tidyverse)
library(rvest)

url <- 'https://everout.com/portland/events/the-portland-mercurys-holiday-drink-week/e131707/'
webpage <- read_html(url)

# get restaurant and drink names
restaurants <- html_nodes(webpage, 'h4') %>% html_text2()
drinks <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to drink descriptions
drink_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# loop through drink links, get info, build drink data table
drink_week <- tibble()

for (i in 1:length(drink_links)) {
  url <- drink_links[i]
  webpage <- read_html(url)
  info <- html_nodes(webpage, '.mb-5 span') %>% html_text2()
  
  
  temp_tib <- tibble(restaurant = restaurants[i],
                     drink = drinks[i],
                     info = info)
  
  drink_week <- drink_week %>% bind_rows(temp_tib)
}

# clean
drink_week <- drink_week %>%
  separate(info, into = c("trash","ingredients", "description", "address_hours"), sep = "What's In It: |What They Say: |Address/Hours of Availability: ") %>% 
  separate(address_hours, into = c("address","hours"), sep = "/") %>% 
  select(-trash)

# save
write_csv(x = drink_week, file = "drink_week_2022.csv")
