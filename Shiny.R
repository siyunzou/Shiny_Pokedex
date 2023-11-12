library(shiny)

ui <- fluidPage(
  titlePanel("Pokedex"),
  
  fluidRow(
    column(4, wellPanel(
      selectInput("picture", "Picture:",
                   c("bulbasaur", "ivysaur", "venusaur"))
    )),
    column(4,
           imageOutput("image2")
    )
  )
)

library(png)

server <- function(input, output, session) {
  
  # image2 sends pre-rendered images
  output$image2 <- renderImage({
    if (is.null(input$picture))
      return(NULL)
    
    if (input$picture == "bulbasaur") {
      return(list(
        src = "images/001.png",
        contentType = "images/png",
        alt = "bulbasaur"
      ))
    } else if (input$picture == "ivysaur") {
      return(list(
        src = "images/002.png",
        filetype = "images/png",
        alt = "This is an ivysaur"
      ))
    } else if (input$picture == "venusaur") {
      return(list(
        src = "images/003.png",
        filetype = "images/png",
        alt = "This is an venusaur"
      ))
    }
    
  }, deleteFile = FALSE)
}

shinyApp(ui, server)

