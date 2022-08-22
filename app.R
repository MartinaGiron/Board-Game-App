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
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
df <- data.frame(lapply(details[,12:19], function(x) str_remove_all(x,"[^[:^punct:],]")))
details <- details %>%
  select(!starts_with("boardgame")) %>%
  cbind(., df)

boardgames <- ratings %>%
  select(id, average, users_rated, thumbnail, rank) %>%
  full_join(., details, by = "id")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Board Game Explorer"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(5, 
            selectizeInput("game1",
                        "Select a game to compare",
                        choices = c("No game selected", boardgames$primary),
                        selected = "No game selected"),
              
            textOutput("game1"),
            textOutput("rank"),
            textOutput("rating")
        ),

        # Show a plot of the generated distribution
        column(5,
               selectizeInput("game2",
                              "Select a game to compare",
                              choices = c("No game selected", boardgames$primary),
                              selected = "No game selected"),
               
               textOutput("game2"),
               textOutput("rank2"),
               textOutput("rating2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  year <- reactive({
    boardgames %>%
      filter(primary == input$game1) %>%
      select(yearpublished) %>% unlist(.)
  })
  
  rank <- reactive({
    boardgames %>%
      filter(primary == input$game1) %>%
      select(rank) %>% unlist(.)
  })
  
  maxrank <- reactive({
    max(boardgames$rank)
  })
  
  rating <- reactive({
    boardgames %>%
      filter(primary == input$game1) %>%
      select(average) %>% unlist(.)
  })
  
  rating_by <- reactive({
    boardgames %>%
      filter(primary == input$game1) %>%
      select(users_rated) %>% unlist(.)
  })
  
  
  # game 2
  year2 <- reactive({
    boardgames %>%
      filter(primary == input$game2) %>%
      select(yearpublished) %>% unlist(.)
  })
  
  rank2 <- reactive({
    boardgames %>%
      filter(primary == input$game2) %>%
      select(rank) %>% unlist(.)
  })
  
  
  rating2 <- reactive({
    boardgames %>%
      filter(primary == input$game2) %>%
      select(average) %>% unlist(.)
  })
  
  rating_by2 <- reactive({
    boardgames %>%
      filter(primary == input$game2) %>%
      select(users_rated) %>% unlist(.)
  })
  #outputs
  
  output$game1 <- renderText({
    if(input$game1 != "No game selected"){
      paste0(input$game1, " (", year(), ")")
    }
    
  }
  )
  
  output$rank <- renderText({
    if(input$game1 != "No game selected"){
      paste0("Rank ", rank(), " of ", maxrank())
    }
  })
  
  output$rating <- renderText({
    if(input$game1 != "No game selected"){
      paste0("Average rating of ", rating(), " by ", rating_by(), " users")
    }
  })
  
  #game 2
  
  output$game2 <- renderText({
    if(input$game2 != "No game selected"){
      paste0(input$game2, " (", year2(), ")")
    }
    
  }
  )
  
  output$rank2 <- renderText({
    if(input$game2 != "No game selected"){
      paste0("Rank ", rank2(), " of ", maxrank())
    }
  })
  
  output$rating2 <- renderText({
    if(input$game2 != "No game selected"){
      paste0("Average rating of ", rating2(), " by ", rating_by2(), " users")
    }
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
