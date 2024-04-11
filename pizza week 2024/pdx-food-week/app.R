#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(DT)
library(googlesheets4)
gs4_deauth()


load("for_shiny.RData")
pizza_week <- pizza_week %>% mutate(interest_level = 5)

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




# Define UI for application that draws a histogram
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
            checkboxGroupInput(inputId = "disp_cols", label = "Choose which columns to display:", choices = c("restaurant","pizza","toppings","address","interest_level","cluster","image","hours","meat_veggie","veggie_available","vegan_available","gf_available","whole_slice","minors","takeout","delivery","purchase_limit","availability_limit"),
                               selected = c("restaurant","pizza","toppings","address","interest_level","cluster", "image")),
            width = 2), # designate sidebar width
        
        
        # Main Panel
        mainPanel(
            
            plotlyOutput("map"),
            
            tabsetPanel(
                tabPanel("Pizzas",
                         br(),
                         p("A filtered list of pizzas that match your criteria, automatically sorted by cluster. Edit your criteria and which columns to display using options in the sidebar on the left. Edit the interest_level column in the Cast Your Votes tab."),
                         downloadButton('download1', "Download Table"),
                         DTOutput("pizza_db")),
                
                tabPanel("Cast Your Votes",
                         br(),
                         p("A list of all pizzas offered this pizza week, sorted alphabetically by restaurant."),
                         wellPanel(fluidRow(HTML('<p style="font-size:14px;margin:15px"> <EM><B>Optional</B> (see more information in the Instructions tab):</EM></p>')),
                                   fluidRow(column(width = 6, textInput(inputId = "gsheet", label = "If you have votes on a Google sheet, paste URL below:", placeholder = dummy_g <- "https://docs.google.com/spreadsheets/d/xxxxxxxxx/edit?usp=sharing")),
                                            column(width = 2, selectInput(inputId = "vote_g", label = "Use Google sheet?", choices = c("Yes","No"), selected = "No", multiple = FALSE)),
                                            column(width = 3, br(), actionButton(inputId = "go2", label = "Update")))),
                         downloadButton('download2', "Download Votes"),
                         DTOutput("vote_db")),
                
                tabPanel("View Hours",
                         br(),
                         p("A filtered list of pizzas that match your criteria, sorted by cluster. Designed to help you plan which day of the week to hit up each cluster."),
                         downloadButton('download3', "Download Hours"),
                         dataTableOutput("schedule")),
                
                tabPanel("Instructions",
                         includeHTML("instructions.html")))
        
                
                )))




server <- function(input, output) {
    
    
    ########################### VOTES ########################### 
    
    # download button for votes
    output$download2 <- downloadHandler(filename = function() {"votes.csv"},
                                        content = function(fname) {write.csv(v$data %>% select(restaurant, pizza, toppings, address, interest_level), fname)})
    
    # for casting votes
    v <- reactiveValues(data = pizza_week %>% select(restaurant, pizza, toppings, address, interest_level, image))
    
    
    # display vote datatable
    output$vote_db <- renderDT({
        
        v$data %>%
            datatable(editable = TRUE, escape = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))

    })
    
    observeEvent(input$go2,
                 if (input$vote_g == "Yes") {
                     external_g <- read_sheet(input$gsheet) %>% select(pizza, restaurant, address, interest_level)
                     v$data <- pizza_week %>% select(-interest_level) %>% inner_join(external_g) %>% select(restaurant, pizza, toppings, address, interest_level, image)
                     output$vote_db <- renderDT({
                         
                         v$data %>%
                             datatable(editable = TRUE, escape = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))

                     
                     })
                 } else {
                     v$data <- pizza_week %>% select(restaurant, pizza, toppings, address, interest_level, image)
                     output$vote_db <- renderDT({
                         
                         v$data %>%
                             datatable(editable = TRUE, escape = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
                     })}
    )
    
    # track changes
    observeEvent(input$vote_db_cell_edit, {
        #get values
        info = input$vote_db_cell_edit
        i = as.numeric(info$row)
        j = as.numeric(info$col)
        k = as.numeric(info$value)
        
        #write values to reactive
        v$data[i,j] <- k
    })
    
    
    
    ########################### FILTERED TABLE ########################### 
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

    # filter, cluster
    filter_db <- reactive({
        votes <- pizza_week %>% select(-interest_level) %>%
            filter(
                grepl(pattern = mvv(), x = meat_veggie_vegan),
                grepl(pattern = ifelse(input$gluten == "All", ".", input$gluten), x = gf_available),
                grepl(pattern = slice_pie(), x = whole_slice),
                grepl(pattern = ifelse(input$takeout == "Takeout", "Yes", "."), x = takeout),
                grepl(pattern = ifelse(input$delivery == "Delivery", "Yes", "."), x = delivery),
                grepl(pattern = ifelse(input$minors == "Either", ".", input$minors), x = minors)) %>%
            inner_join(v$data) %>%
            filter(interest_level >= input$vote)


        # clusters
        coords_scale <- votes %>% select(latitude, longitude) %>% scale()
        km <- kmeans(coords_scale, input$days, nstart = 4)
        votes$cluster <- km$cluster %>% as_factor()
        votes <- votes %>% arrange(cluster)

        return(votes)
    })
    
    
    # download filtered table
    output$download1 <- downloadHandler(filename = function() {"pizzas.csv"},
        content = function(fname) {write.csv(filter_db() %>% select(input$disp_cols), fname)})
    
    # display filtered results
    output$pizza_db <- renderDT({

        # display table
        filter_db() %>%
            select(input$disp_cols) %>%
            datatable(escape = FALSE, rownames = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
    })
    
    observeEvent(input$go2,
                 output$pizza_db <- renderDT({
                     
                     # display table
                     filter_db() %>%
                         select(input$disp_cols) %>%
                         datatable(escape = FALSE, rownames = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
                 }))

    
    
    
    
    ########################### HOURS ########################### 
    
    # show hours for filtered results
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
    
    output$schedule <- renderDataTable({
        votes <- filter_db()
        
        # display table
        votes %>%
            select(cluster, restaurant, pizza, address, hours) %>%
            arrange(cluster) %>%
            datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
    })
    
    ########################### MAP ########################### 
    output$map <- renderPlotly({
        input$go
        
        votes <- isolate(filter_db())
        
        # plot map of clusters
        
        p <- votes %>%
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
