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
wing_week <- wing_week %>% mutate(interest_level = 5,
                                      toppings = paste0(toppings, " ", description)) %>% 
    mutate(veggie = case_when(is.na(veggie) ~ "Yes",
                              TRUE ~ veggie)) %>% 
    select(-description)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Wing Week 2023"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            actionButton("go", label = "Update Map"),
            sliderInput(inputId = "days", label = "How many days long is your wing crawl?", min = 1, max = 7, value = 3),
            sliderInput(inputId = "vote",label = "Only show wings I am interested in. Interest threshold:", min = 1, max = 5, value = 3),
            checkboxGroupInput(inputId = "meat_veg", label = "Meat or Vegetarian?", choices = c("Meat", "Vegetarian"), selected = c("Meat", "Vegetarian")),
            selectInput(inputId = "gluten", label = "Gluten-Free Option?", choices = c("Gluten-Free Only", "Either"), selected = "Either", multiple = FALSE),
            selectInput(inputId = "takeout", label = "Dine In or Takeout?", choices = c("Takeout", "Either"), selected = "Either", multiple = FALSE),
            selectInput(inputId = "delivery", label = "Offers Delivery?", choices = c("Delivery", "Don't Care"), selected = "Don't Care", multiple = FALSE),
            selectInput(inputId = "minors", label = "Allows Minors?", choices = c("Yes", "No", "Either"), selected = "Either", multiple = FALSE),
            checkboxGroupInput(inputId = "disp_cols", label = "Choose which columns to display:", choices = c("restaurant","wing","toppings","address","interest_level","cluster","hours","gluten_free","chick_veggie","veggie","minors","takeout","delivery","purchase_limit"),
                               selected = c("restaurant","wing","toppings","address","interest_level","cluster")),
            width = 2), # designate sidebar width
        
        
        # Main Panel
        mainPanel(
            
            plotlyOutput("map"),
            
            tabsetPanel(
                tabPanel("Wings",
                         downloadButton('download1', "Download Table"),
                         DTOutput("wing_db")),
                
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
                                        content = function(fname) {write.csv(v$data %>% select(restaurant, wing, toppings, address, interest_level), fname)})
    
    # for casting votes
    v <- reactiveValues(data = wing_week %>% select(restaurant, wing, toppings, address, interest_level))
    
    
    # display vote datatable
    output$vote_db <- renderDT({
        
        v$data %>%
            datatable(editable = TRUE,
                      # editable = list(target = 'column', disable = list(columns = c(1:4))),
                      options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
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

    veg_pattern <- reactive({
        # meat only
        if ("Meat" %in% input$meat_veg == TRUE & "Vegetarian" %in% input$meat_veg == FALSE) {
            vp <- "."
        } else if ("Meat" %in% input$meat_veg == FALSE & "Vegetarian" %in% input$meat_veg == TRUE) {
            vp <- "Yes|^$"
        } else {
            vp <-  "."
        }
        return(vp)
    })
    
    # filter, cluster
    filter_db <- reactive({
        votes <- wing_week %>% select(-interest_level) %>% 
            filter(
                grepl(pattern = veg_pattern(), x = veggie),
                grepl(pattern = ifelse(input$takeout == "Takeout", "Yes|Maybe", "."), x = takeout),
                grepl(pattern = ifelse(input$gluten == "Either", ".", "Yes"), x = gluten_free),
                grepl(pattern = ifelse(input$delivery == "Delivery", "Yes|Maybe", "."), x = delivery),
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
    output$download1 <- downloadHandler(filename = function() {"wings.csv"},
                                        content = function(fname) {write.csv(filter_db() %>% select(input$disp_cols), fname)})
    
    # display filtered results
    output$wing_db <- renderDT({
        
        # display table
        filter_db() %>%
            select(input$disp_cols) %>%
            datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 100))
    })
    
    
    ########################### HOURS ########################### 
    
    # show hours for filtered results
    output$download3 <- downloadHandler(
        filename = function() {
            "hours.csv"
        },
        content = function(fname) {
            write.csv(
                filter_db() %>% select(cluster, restaurant, wing, address, hours) %>% arrange(cluster),
                fname
            )
        }
    )
    
    output$schedule <- renderDataTable({
        votes <- filter_db()
        
        # display table
        votes %>%
            select(cluster, restaurant, wing, address, hours) %>%
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
                    wing,
                    paste("Interest Level: ", interest_level),
                    str_wrap(toppings, width = 35),
                    str_wrap(address, width = 35),
                    str_wrap(hours, width = 35),
                    paste("Takeout? ", str_wrap(takeout, width = 35)),
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
