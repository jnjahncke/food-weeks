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

load("for_shiny.RData")
sandwich_week <- sandwich_week %>% mutate(interest_level = 5, 
                                          toppings = paste0(toppings, " ", description)) %>% 
    select(-description)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Sandwich Week 2023"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            actionButton("go", label = "Update Map"),
            sliderInput(inputId = "days", label = "How many days long is your sandwich crawl?", min = 1, max = 7, value = 1),
            sliderInput(inputId = "vote",label = "Only show sandwiches I am interested in. Interest threshold:", min = 1, max = 5, value = 3),
            checkboxGroupInput(inputId = "meat_veg", label = "Meat or Vegetarian?", choices = c("Meat", "Vegetarian", "Pescatarian", "Vegan"), selected = c("Meat", "Vegetarian", "Pescatarian", "Vegan")),
            selectInput(inputId = "takeout", label = "Dine In or Takeout?", choices = c("Takeout", "Either"), selected = "Either", multiple = FALSE),
            selectInput(inputId = "delivery", label = "Offers Delivery?", choices = c("Delivery", "Don't Care"), selected = "Don't Care", multiple = FALSE),
            selectInput(inputId = "minors", label = "Allows Minors?", choices = c("Yes", "No", "Either"), selected = "Either", multiple = FALSE),
            checkboxGroupInput(inputId = "disp_cols", label = "Choose which columns to display:", choices = c("restaurant","sandwich","toppings","address","interest_level","cluster","hours","meat_veggie","minors","takeout","delivery","purchase_limit","available_limit"),
                               selected = c("restaurant","sandwich","toppings","address","interest_level","cluster")),
            width = 2), # designate sidebar width
        
        
        # Main Panel
        mainPanel(
            
            plotlyOutput("map"),
            
            tabsetPanel(
                tabPanel("Sandwiches",
                         downloadButton('download1', "Download Table"),
                         DTOutput("sandwich_db")),
                
                tabPanel("Cast Your Votes",
                         downloadButton('download2', "Download Votes"),
                         DTOutput("vote_db")),
                
                tabPanel("View Hours",
                         downloadButton('download3', "Download Hours"),
                         dataTableOutput("schedule")),
                
                tabPanel("Instructions",
                         includeHTML("instructions.html")))
            
            
        )))




server <- function(input, output) {
    
    
    ########################### VOTES ########################### 
    
    # download button for votes
    output$download2 <- downloadHandler(filename = function() {"votes.csv"},
                                        content = function(fname) {write.csv(v$data %>% select(restaurant, sandwich, toppings, address, interest_level), fname)})
    
    # for casting votes
    v <- reactiveValues(data = sandwich_week %>% select(restaurant, sandwich, toppings, address, interest_level))
    
    
    # display vote datatable
    output$vote_db <- renderDT({
        
        v$data %>%
            datatable(editable = TRUE,
                      # editable = list(target = 'column', disable = list(columns = c(1:4))),
                      options = list(lengthMenu = c(5, 10, 35), pageLength = 35))
    })
    
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
    meat_veg_pattern <- reactive({
        diet_pattern <-  "holder"
        for (diet in input$meat_veg) {
            diet_pattern <- paste0(diet_pattern, "|", diet)
        }
        if ("Vegan" %in% input$meat_veg) {
            diet_pattern <- paste0(diet_pattern, "|vegan")
        }
        return(diet_pattern)
    })
    
    # filter, cluster
    filter_db <- reactive({
        votes <- sandwich_week %>% select(-interest_level) %>% 
            filter(
                grepl(pattern = meat_veg_pattern(), x = meat_veggie),
                grepl(pattern = ifelse(input$takeout == "Takeout", "Yes", "."), x = takeout),
                grepl(pattern = ifelse(input$delivery == "Delivery", "Yes", "."), x = delivery),
                grepl(pattern = ifelse(input$minors == "Either", ".", input$minors), x = minors)) %>% 
            inner_join(v$data) %>% 
            filter(interest_level >= input$vote)
        
        
        # clusters
        coords_scale <- votes %>% select(latitude, longitude) %>% scale()
        km <- kmeans(coords_scale, input$days, nstart = 4)
        votes$cluster <- km$cluster %>% as_factor()
        
        return(votes)
    })
    
    # download filtered table
    output$download1 <- downloadHandler(filename = function() {"sandwiches.csv"},
                                        content = function(fname) {write.csv(filter_db() %>% select(input$disp_cols), fname)})
    
    # display filtered results
    output$sandwich_db <- renderDT({
        
        # display table
        filter_db() %>%
            select(input$disp_cols) %>%
            datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 35), pageLength = 35))
    })
    
    
    ########################### HOURS ########################### 
    
    # show hours for filtered results
    output$download3 <- downloadHandler(
        filename = function() {
            "hours.csv"
        },
        content = function(fname) {
            write.csv(
                filter_db() %>% select(cluster, restaurant, sandwich, address, hours) %>% arrange(cluster),
                fname
            )
        }
    )
    
    output$schedule <- renderDataTable({
        votes <- filter_db()
        
        # display table
        votes %>%
            select(cluster, restaurant, sandwich, address, hours) %>%
            arrange(cluster) %>%
            datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 35), pageLength = 35))
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
                    sandwich,
                    paste("Interest Level: ", interest_level),
                    str_wrap(toppings, width = 35),
                    str_wrap(address, width = 35),
                    str_wrap(hours, width = 35),
                    paste("Takeout? ", str_wrap(takeout, width = 35)),
                    paste("Availability limit? ", available_limit),
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
