#library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)
library(rsconnect)

library(tidyverse)
library(dplyr)

pokeframe_df <- read_csv("pokemon_df.csv")

pokeframe_df <- pokeframe_df %>% arrange(id, pokemon)

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
             
             dataTableOutput("table")),
    # third tab: some statistical visualization
    tabPanel("Visualizations",
             titlePanel("Data Visualizations"),
              mainPanel(plotlyOutput("plot1"),
                         plotlyOutput("plot2"))
              )
             
  ))

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
  
  # third tab: rending plots
  output$plot1 <- renderPlotly({
    p <- ggplot(pokeframe_df, aes(weight, height, color=pokemon)) 
    p <- p + geom_point() + ggtitle("Height and Weight of Pokémons") +
      theme(legend.position="none")
    
    ggplotly(p)
    
  })
  
  output$plot2 <- renderPlotly({
    p2 <- ggplot(pokeframe_df, aes(type, fill = "blue")) 
    p2 <- p2 + geom_bar() + coord_flip() + ggtitle("Types of Pokémons") +
      theme(legend.position="none")
    
    ggplotly(p2)
    
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

# pokemon data that can be filter 
# overall data analysis 


