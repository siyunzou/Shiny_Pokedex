#library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(plotly)
library(bslib)
library(rsconnect)

library(tidyverse)
library(dplyr)

# reading in data via csv pulled from API
pokeframe_df <- read_csv("pokemon_df.csv")
# arranging Pokemon in order by id
pokeframe_df <- pokeframe_df %>% arrange(id, pokemon)
# setting up a list for the Shiny App
pokemon_list <- as.list(pokeframe_df$pokemon)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tabsetPanel(
    tabPanel("Pokédex", 
             titlePanel("Pokédex"),
             
             fluidRow(
               column(4, wellPanel(
                 selectInput("picture", "Select a Pokémon:",
                             pokemon_list)
               )),
               column(4,
                      imageOutput("image2")
               )
             ),
             textOutput("height"),
             textOutput("weight"),
             textOutput("type"),
             textOutput("abilities"),
             textOutput("moves")
             
    ),
    # second tab: the full dataset for viewers to access/filter/search
    tabPanel("Full Dataset",
             titlePanel("First Generation Pokémon Dataset"),
             # adding in a download button for the dataset
             downloadButton('download', 'Download the data'),
             dataTableOutput("table")),
    # third tab: some statistical visualization
    tabPanel("Visualizations",
             titlePanel("Data Visualizations"),
             
             basicPage(
               sidebarLayout(
               sidebarPanel(
                   selectInput("mydropdown", "Pokemon Type:", choices = c("All", unique(pokeframe_df$type)), 
                               )
                   ),
               mainPanel(
                   plotlyOutput("plot1"),
                   plotlyOutput("plot2")
               )
             )
             )
    )
              )
             
  )

library(png)

server <- function(input, output, session) {
  
  # load in dataset
  pokeframe_df <- read_csv("pokemon_df.csv")
  
  # render image for each pokemon
  output$image2 <- renderImage({
    selected_pokemon <- pokeframe_df %>%
      filter(input$picture == pokemon)
    
  path <- paste0("www/", selected_pokemon$id, ".png")
  return(list(
    src = path,
    contentType = "images/png"))
  })
  # second tab: rendering data table
  output$table <- renderDataTable(pokeframe_df, options = list(pageLength = 15))
  
  output$download <- downloadHandler(
    filename = function(){"pokemon_df.csv"},
    content = function(fname){
      write.csv(pokeframe_df, fname)
    }
  )
  
  # third tab: rending plots
  
  observe({
    print(input$mydropdown)
  })
  
  df2 <- reactive({
    df <- (input$mydropdown == "All"|pokeframe_df$type == input$mydropdown)
    pokeframe_df[df,,drop=FALSE]
  })
  
  # plot 1
  output$plot1 <- renderPlotly({
    data_filtered <- df2()
    plot_ly(data_filtered, x = ~data_filtered$weight, y = ~data_filtered$height, 
            type = 'scatter', color = ~data_filtered$pokemon) %>%
      layout(title = "Height and Weight of Pokémons",
             xaxis = list(title = "weight"),
             yaxis = list(title = "height")
        
      )
    # p <- ggplot(data_filtered, aes(weight, height, color=pokemon)) 
    # p <- p + geom_point() + ggtitle("Height and Weight of Pokémons") +
    #   theme(legend.position="none")
    # 
    # ggplotly(p)
    })

    # plot 2  

  output$plot2 <- renderPlotly({
    data_filtered <- df2()
    plot_ly(data_filtered, x = ~data_filtered$type, y = ~data_filtered$abilities, 
            type = 'scatter') %>%
      layout(title = "Abilities and Types of Pokémons",
             xaxis = list(title = "type"),
             yaxis = list(title = "abilities")
             
      )
    
  #   output$plot2 <- renderPlotly({
  #     p2 <- ggplot(data_filtered, aes(fct_infreq(type), fill = "blue")) 
  #     p2 <- p2 + geom_bar() + coord_flip() + ggtitle("Types of Pokémons") +
  #       theme(legend.position="none") + xlab("Types")
  #     
  #     ggplotly(p2)
  })
  
  output$height <- renderText({
    selected_pokemon <- pokeframe_df %>%
      filter(input$picture == pokemon)
    
    paste0("Height (decimetres): ", selected_pokemon$height)
  })
  
  output$weight <- renderText({
    selected_pokemon <- pokeframe_df %>%
      filter(input$picture == pokemon)
    
    paste0("Weight (hectograms): ", selected_pokemon$weight)
  })
  
  output$type <- renderText({
    selected_pokemon <- pokeframe_df %>%
      filter(input$picture == pokemon)
    
    paste0("Type: ", selected_pokemon$type)
  })
  
  output$abilities <- renderText({
    selected_pokemon <- pokeframe_df %>%
      filter(input$picture == pokemon)
    
    paste0("Abilities: ", selected_pokemon$abilities)
  })
  
  output$moves <- renderText({
    selected_pokemon <- pokeframe_df %>%
      filter(input$picture == pokemon)
    
    paste0("Top 5 Moves : ", selected_pokemon$moves)
  })
  
  }

shinyApp(ui, server)



