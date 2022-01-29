library(tidyverse)
library(rvest)

url <- 'https://www.dumplingweek.com/'
webpage <- read_html(url)

restaurant <- html_nodes(webpage,'h2') %>% html_text2() %>% str_replace_all("[\n]" , " ")
content <- html_nodes(webpage, '.span-5+ .span-5 .sqs-block-content') %>% html_text2()

dumpling_week <- tibble(restaurant = restaurant,
                        content = content)
# cleaning
dumpling_week <- dumpling_week %>% 
  separate(content, into = c("url","address","dining","dumpling"), sep = "\n\n") %>% 
  separate(dining, into = c("trash","dining"), sep = ": ", extra = "merge") %>% 
  separate(dumpling, into = c("trash","dumpling"), sep = ": ", extra = "merge") %>% 
  select(restaurant, url, dumpling, address, dining)

write_csv(x = dumpling_week, path = "dumpling_week_2022.csv")
