library(tidyverse)
library(rvest)

url <- 'https://everout.com/portland/events/the-portland-mercurys-highball-2023/e136496/'
webpage <- read_html(url)

# get restaurant and highball names
restaurants <- html_nodes(webpage, 'h4') %>% html_text2()
drinks <- html_nodes(webpage, 'h3 a') %>% html_text2()

# list of links to highball descriptions
rest_links <- html_nodes(webpage, 'h3 a') %>% html_attr('href')

# loop through highball links, get info, build highball data table
highball_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  info <- html_nodes(webpage, '.mb-5 span') %>% html_text2() %>% str_split_1("What It's Called:|What's In It:|What They Say About It:|Where and When to Get It:|Minors Allowed Inside[?]")
  
  ingredients_temp <- info[3] %>% str_squish()
  address_hours_temp <- info[5] %>% str_split_1(",| / ") %>% str_squish()
  minors_temp <- info[6] %>% str_squish()
  
  temp_tib <- tibble(restaurant = restaurants[i],
                     drink = drinks[i],
                     ingredients = ingredients_temp,
                     address = address_hours_temp[2],
                     hours = address_hours_temp[3],
                     minors = minors_temp)
  
  highball_week <- highball_week %>% bind_rows(temp_tib)
}

write_csv(x = highball_week, path = "highball_week_2023.csv")