library(tidyverse)
library(rvest)

url <- 'https://www.dumplingweek.com/'
webpage <- read_html(url)

restaurant <- html_nodes(webpage,'h2') %>% html_text2() %>% str_replace_all("[\n]" , " ")
content <- html_nodes(webpage, '.span-5+ .span-5 .sqs-block-content , .span-4+ .span-4 .sqs-block-html') %>% html_text2()

dumpling_week <- tibble(restaurant = restaurant,
                        content = content)
# cleaning
dumpling_week <- dumpling_week %>% 
  separate(content, into = c("url", "address", "dining", "dumpling"), sep = "\n\n")

dumpling_weekA <- dumpling_week %>% filter(restaurant == "Farm To Fit") %>% 
  select(-dumpling) %>% 
  rename(dumpling = dining, dining = address)

dumpling_week <- dumpling_week %>% filter(restaurant != "Farm To Fit") %>%
  full_join(dumpling_weekA) %>% 
  separate(dumpling, into = c("trash","dumpling"), sep = ": ") %>% 
  select(restaurant, url, dumpling, address, dining)

write_csv(x = dumpling_week, path = "dumpling_week_2023.csv")
