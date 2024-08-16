library(tidyverse)
library(rvest)
library(googlesheets4)

url <- 'https://everout.com/portland/events/mercury-burger-week-2024/e180638/'
webpage <- read_html(url)

# get restaurant and burger names
restaurants <- html_nodes(webpage, '.text-center h4') |> html_text2()
burgers <- html_nodes(webpage, 'h3 a') |> html_text2()
img_links <- html_elements(webpage, '.img-fluid') %>% html_attr("src")

# list of links to burger descriptions
rest_links <- html_nodes(webpage, 'h3 a') |> html_attr('href')

# label dictionary - will be used to name columns
label_dict <- c("What It's Called:" = "burger",
                "What's On It:" = "toppings",                 
                "What They Say About It:" = "description",
                "Where and When To Get It:" = "address_hours",      
                "Jim Beam Drink Special:" = "JB_special",
                "The Fine Print!" = "fine_print",
                "Allow Minors?" = "minors",               
                "Allow Takeout?" = "takeout",
                "Allow Delivery?" = "delivery",                   
                "Purchase Limit?" = "purchase_limit",
                "Meat or Vegetarian?" = "meat_veggie",           
                "Available Gluten-Free?" = "gluten_free",
                "Available Vegetarian?" = "veggie",       
                "Available Vegan?" = "vegan")

# loop through burger links, get info, build burger data table
burger_week <- tibble()
for (i in 1:length(rest_links)) {
  url <- rest_links[i]
  webpage <- read_html(url)
  
  labels <- html_nodes(webpage, '.mb-5 > strong') |> html_text2() |> str_squish()
  info <- html_nodes(webpage, '.mb-5 span') |> html_text2() |> str_squish()
  info <- info[info != "The Fine Print!"]
  
  if (length(labels) == 0) {
    labels <- html_nodes(webpage, '.mb-5 strong') |> html_text2() |> str_squish()
  }
  labels <- labels[labels != "The Fine Print!"]
  labels <- labels[labels != "AND"]
  

  
  # build tibble
  if (length(labels) != length(info)) {
    description <- html_nodes(webpage, ".description") |> html_text2() |> 
      str_split("\\n")
    description <- description[[1]]
    description <- description[description != ""]
    description <- description[description != "The Fine Print!"]
    
    info2 <- c()
    for (j in seq(1,length(description))) {
      info_temp <- str_split_1(description[j], labels[j])
      info2 = c(info2,info_temp[2] |> str_squish())
    }
    
    info <- info2
  } 
  
  temp_tib = tibble(label = labels, info = info) |> 
    mutate(label2 = label_dict[label]) |> 
    select(-label) |> rename(label = label2) |> 
    pivot_wider(names_from = label, values_from = info) |> 
    mutate(image = img_links[i])
  


  if (dim(burger_week)[1] == 0) {
    burger_week <- burger_week |> bind_rows(temp_tib)
  } else {
    burger_week <- burger_week |> full_join(temp_tib)
    }
  
  rm(info, url, webpage, labels, temp_tib, info2, info_temp, i, j, description)
}

# cleaning
burger_week <- burger_week |> 
  mutate(description = case_when(is.na(description) ~ `NA`,
                                 TRUE ~ description),
         address_hours = case_when(is.na(address_hours) ~ `NA`,
                                 TRUE ~ address_hours)) |> 
  select(-`NA`)


# export
save(burger_week, file = "burger_week.RData")
write_csv(x = burger_week |> select(-image), file = "burger_week_2024.csv")

# save to google sheets so we can vote:
formula <- '=IMAGE("'
end <- '")'
burger_week_g <- burger_week |> 
  mutate(image = paste0(formula, image, end)) |> 
  select(burger, toppings, description, image, everything())
gs4_create("burger-week-2024", sheets = burger_week_g)