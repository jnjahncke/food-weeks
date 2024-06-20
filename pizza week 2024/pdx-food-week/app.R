
library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(DT)
library(googlesheets4)
gs4_deauth()


# load data 
load("for_shiny.RData")
pizza_week <- pizza_week %>% mutate(interest_level = 5)

### Cleaning data to fit the needs of the app ###

# combo meat_veg with veggie_sub and vegan_sub
# meat_veggie options: "Meat", "Vegan", "Meat, Vegan", "Vegetarian"
# veggie_sub options: "Yes", "No", NA -  here NA means that it is already vegetarian
# vegan_sub options: "Yes", "No", NA - here NA means that it is already vegan
# make a new column: meat_veggie_vegan that lists options
pizza_week <- pizza_week %>% mutate(veggie_sub = replace_na(veggie_sub, "Yes"),
                                    vegan_sub = replace_na(vegan_sub, "Yes"))

meat_veg_func <- function(meat_veggie, veggie_sub, vegan_sub) {
    meat <- if (grepl(x = meat_veggie, pattern = "[Mm]eat")) {TRUE} else {FALSE}
    veggie <- if (grepl(x = meat_veggie, pattern = "[Vv]egetarian") | grepl(x = veggie_sub, pattern = "[Yy]es") & !grepl(x = meat_veggie, pattern = "[Vv]egan")) {TRUE} else {FALSE}
    vegan <- if (grepl(x = meat_veggie, pattern = "[Vv]egan") | grepl(x = vegan_sub, pattern = "[Yy]es")) {TRUE} else {FALSE}
    mvv <- c(meat, veggie, vegan)
    
    p <- c()
    nms <- c("Meat", "Vegetarian", "Vegan")
    i = 1
    for (x in mvv) {
        if (x) {p <- append(p,nms[i])}
        i = i + 1}
    
    return(paste(unlist(p), collapse=', '))
}

pizza_week$meat_veggie_vegan <- mapply(meat_veg_func, pizza_week$meat_veggie, pizza_week$veggie_sub, pizza_week$vegan_sub)

# combo gf with gf_sub into
# all gf are No
# all gf_sub have either a yes or no
# just rename gf_sub into gf_available

pizza_week <- pizza_week %>% rename(gf_available = gf_sub, veggie_available = veggie_sub, vegan_available = vegan_sub)

# change image column to display the image
# '<img src="URL" height="200"></img>
first <- '<img src="'
last <- '" height="200"></img>'
pizza_week <- pizza_week %>% mutate(image = paste0(first,image,last))



# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Pizza Week 2024"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            actionButton("go", label = "Update Map"),
            sliderInput(inputId = "days", label = "How many days long is your pizza crawl?", min = 1, max = 7, value = 7),
            sliderInput(inputId = "vote",label = "Only show pizzas I am interested in. Interest threshold:", min = 1, max = 5, value = 3),
            checkboxGroupInput(inputId = "meat_veg", label = "Meat, Vegetarian, or Vegan?", choices = c("Meat", "Vegetarian", "Vegan"), selected = c("Meat","Vegetarian","Vegan")),
            selectInput(inputId = "gluten", label = "Gluten free?", choices = c("Yes", "No", "All"), selected = "All", multiple = FALSE),
            selectInput(inputId = "slice_pie", label = "Slice or Whole Pie?", choices = c("Slice", "Whole Pie", "Either"), selected = "Either", multiple = FALSE),
            selectInput(inputId = "takeout", label = "Dine In or Takeout?", choices = c("Takeout", "Either"), selected = "Either", multiple = FALSE),
            selectInput(inputId = "delivery", label = "Offers Delivery?", choices = c("Delivery", "Don't Care"), selected = "Don't Care", multiple = FALSE),
            selectInput(inputId = "minors", label = "Allows Minors?", choices = c("Yes", "No", "Either"), selected = "Either", multiple = FALSE),
            checkboxGroupInput(inputId = "disp_cols", label = "Choose which columns to display:", choices = c("restaurant","pizza","toppings","address","interest_level", "cluster", "image","hours","meat_veggie","veggie_available","vegan_available","gf_available","whole_slice","minors","takeout","delivery","purchase_limit","availability_limit"),
                               selected = c("restaurant","pizza","toppings","address","interest_level", "cluster", "image")),
            width = 2), # designate sidebar width
        
        
        # Main Panel
        mainPanel(
            
            plotlyOutput("map"),
            
            tabsetPanel(
                tabPanel("Pizzas",
                         br(),
                         p("A list of pizza week pizzas. Edit your criteria and which columns to display using options in the sidebar on the left. You can edit the interest_level column by double clicking the cell."),
                         selectInput(inputId = "filtered_all", label = "Choose which pizzas to display in the table below", choices = c("All pizzas", "Only the pizzas that meet my criteria"), selected = "Only the pizzas that meet my criteria", multiple = FALSE),
                         br(),
                         wellPanel(fluidRow(HTML('<p style="font-size:14px;margin:15px"> <EM><B>Optional</B> (see more information in the Instructions tab):</EM></p>')),
                                   fluidRow(column(width = 6, textInput(inputId = "gsheet", label = "If you have votes on a Google sheet, paste URL below:", placeholder = dummy_g <- "https://docs.google.com/spreadsheets/d/xxxxxxxxx/edit?usp=sharing")),
                                            column(width = 2, selectInput(inputId = "vote_g", label = "Use Google sheet?", choices = c("Yes","No"), selected = "No", multiple = FALSE)),
                                            column(width = 3, br(), actionButton(inputId = "go2", label = "Update")))),
                         downloadButton('download1', "Download Table"),
                         DTOutput("pizza_db")),
                
                tabPanel("View Hours",
                         br(),
                         p("A filtered list of pizzas that match your criteria, sorted by cluster. Designed to help you plan which day of the week to hit up each cluster."),
                         downloadButton('download3', "Download Hours"),
                         dataTableOutput("schedule")),
                
                tabPanel("Instructions",
                         includeHTML("instructions.html")))
            
            
        )))



server <- function(input, output) {
    
    ########################### PIZZAS ########################### 
    
    # define reactive functions
    slice_pie <- reactive({
        if (input$slice_pie == "Slice") {
            return("Slice|Both")
        } else if (input$slice_pie == "Whole Pie") {
            return("Whole|Both")
        } else {
            return(".")
        }
    })
    
    mvv <- reactive({
        meat <- if ("Meat" %in% input$meat_veg) {TRUE} else {FALSE}
        veggie <- if ("Vegetarian" %in% input$meat_veg) {TRUE} else {FALSE}
        vegan <- if ("Vegan" %in% input$meat_veg) {TRUE} else {FALSE}
        mvv <- c(meat, veggie, vegan)
        p <- c()
        nms <- c("Meat", "Vegetarian", "Vegan")
        i = 1
        for (x in mvv) {
            if (x) {p <- append(p,nms[i])}
            i = i + 1
        }
        return(paste(unlist(p), collapse='|'))
    })
    
    # keep pizza_week untouched, create `v`, which keeps track of ALL pizzas
    v <- reactiveValues(data = pizza_week %>% mutate(cluster = NA))
    
    # filter, cluster
    get_votes <- function(db) {
        
        # filter table based on sidebar input
        # do NOT filter based on interest_level otherwise those values will be lost
        # this means you must specify filter(interest_level >= input$vote) wherever filter_db() is used later on
        votes <- db %>%
            filter(
                grepl(pattern = mvv(), x = meat_veggie_vegan),
                grepl(pattern = ifelse(input$gluten == "All", ".", input$gluten), x = gf_available),
                grepl(pattern = slice_pie(), x = whole_slice),
                grepl(pattern = ifelse(input$takeout == "Takeout", "Yes", "."), x = takeout),
                grepl(pattern = ifelse(input$delivery == "Delivery", "Yes", "."), x = delivery),
                grepl(pattern = ifelse(input$minors == "Either", ".", input$minors), x = minors))
        
        votes_filtered <- votes %>% filter(interest_level >= input$vote)
        
        # if the number of pizzas fitting your criteria is smaller than the number of requested clusters,
        # update the number of requested clusters to be equal to the number of pizzas in the filtered table
        if (input$days > nrow(votes_filtered)) {
            updateSliderInput(inputId = "days", label = "How many days long is your pizza crawl?", min = 1, max = 7, value = nrow(votes_filtered))
        }
        
        # assign clusters
        coords_scale <- votes_filtered %>% select(latitude, longitude) %>% scale()
        km <- kmeans(coords_scale, input$days, nstart = 1, algorithm = "Lloyd")
        votes_filtered$cluster <- km$cluster %>% as_factor()
        
        # join votes (retained interest_level) with votes_filtered (contains new cluster assignments)
        votes <- votes %>% mutate(cluster = NA) %>% select(-cluster) %>% 
            full_join(votes_filtered)
        
        return(votes)
    }
    
    filter_db <- reactive({
        get_votes(v$data)
    })
    
    all_pizza <- reactive({
        pizza_week %>% select(-interest_level) %>% full_join(filter_db()) %>% replace_na(list(interest_level = 0))
    })
    
    

    # download filtered table
    output$download1 <- downloadHandler(filename = function() {"pizzas.csv"},
                                        content = function(fname) {write.csv(filter_db() %>% select(input$disp_cols), fname)})

    # display table
    output$pizza_db <- renderDT({
        # display filtered data if requested
        if (input$filtered_all == "Only the pizzas that meet my criteria") {
            filter_db() %>%
                filter(interest_level >= input$vote) %>% 
                select(input$disp_cols) %>%
                datatable(editable = TRUE, escape = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
        } else {
            all_pizza() %>% 
                select(input$disp_cols) %>%
                datatable(editable = TRUE, escape = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
        }
    })
    
    # import google sheet
    observeEvent(input$go2,
                 if (input$vote_g == "Yes") {
                     external_g <- read_sheet(input$gsheet) %>% select(pizza, restaurant, address, interest_level)
                     v$data <- v$data %>% select(-interest_level) %>% inner_join(external_g)
                 } else {
                     v$data <- pizza_week
                     }
    )
    
    # track changes
    observeEvent(input$pizza_db_cell_edit, {
        if (input$filtered_all == "Only the pizzas that meet my criteria") {
            v_temp <- filter_db() %>% select(input$disp_cols)
            v_temp <- editData(data = v_temp, info = input$pizza_db_cell_edit)
            v$data <- v$data %>% select(-cluster, -interest_level) %>% full_join(v_temp)
        } else {
            v_temp <- all_pizza() %>% select(input$disp_cols)
            v_temp <- editData(data = v_temp, info = input$pizza_db_cell_edit)
            v$data <- v$data %>% select(-cluster, -interest_level) %>% full_join(v_temp)
            # need to add: if the row you changed is not in filter_db(), CHANGE THE INPUT that does not match
            # ex: if you originally opted for no meat pizzas, but then voted on a meat pizza, check the meat box but keep all the rest of the meat pizzas at interest_level = 0
        }
    })
    
    ########################### HOURS ########################### 
    
    # download hours table
    output$download3 <- downloadHandler(
        filename = function() {
            "hours.csv"
        },
        content = function(fname) {
            write.csv(
                filter_db() %>% select(cluster, restaurant, pizza, address, hours) %>% arrange(cluster),
                fname
            )
        }
    )
    
    # show hours for filtered results
    output$schedule <- renderDataTable({
        filter_db() %>% 
            filter(interest_level >= input$vote) %>% 
            select(cluster, restaurant, pizza, address, hours) %>%
            arrange(cluster) %>%
            datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
    })
    
    ########################### MAP ########################### 
    
    # render map of clusters
    output$map <- renderPlotly({
        input$go

        p <- isolate(filter_db()) %>%
            filter(interest_level >= input$vote) %>% 
            filter(!is.na(cluster)) %>% 
            ggplot() +
            geom_sf(data = pdx) +
            geom_sf(data = river_boundaries, fill = "dodgerblue", color = "transparent", alpha = 0.5) +
            geom_point(
                aes(x = lon, y = lat, color = cluster, text = paste(
                    restaurant,
                    pizza,
                    paste("Interest Level: ", interest_level),
                    str_wrap(toppings, width = 35),
                    str_wrap(address, width = 35),
                    str_wrap(hours, width = 35),
                    paste("Slice or Pie? ", whole_slice),
                    paste("Takeout? ", str_wrap(takeout, width = 35)),
                    paste("Availability limit? ", availability_limit),
                    sep = "<br />"
                )
                ), shape = 19, size = 2, alpha = 0.7) +
            geom_text(aes(x = lon, y = lat, label = interest_level)) +
            theme(axis.title = element_blank())
        ggplotly(p, tooltip = "text")
    })
    
 
}

# Run the application
shinyApp(ui = ui, server = server)