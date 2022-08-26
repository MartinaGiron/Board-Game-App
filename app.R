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
library(DT)
library(plotly)
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
df <- data.frame(lapply(details[,12:19], function(x) str_remove_all(x,"[^[:^punct:],]")))
details <- details %>%
  select(!starts_with("boardgame")) %>%
  cbind(., df)

boardgames <- ratings %>%
  select(id, average, users_rated, thumbnail, rank) %>%
  full_join(., details, by = "id")

boardgames_preview <- read_csv("Boardgame Preview.csv")
category_list <- str_trim(unlist(strsplit(boardgames$boardgamecategory, ","))) %>% unique() 
category_list <- subset(category_list, !is.na(category_list))

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.google.com/specimen/Open+Sans?query=open+sans'); 
      h1 {font - weight: 500;
        text-align:center;}
      h3 {font - weight: 300;text-align:center;}
      p {font - weight: 300;font-family: 'Open Sans'}
      .center {display: block;margin-left: auto;margin-right: auto;width: 90%;}
      .table-class {display: block;margin-left: auto;margin-right: auto;width: 70%;}"
    ))
  ),
    # Application title
    titlePanel(span(
      img(src="LOGO Main.png", 
          width="50px"),
                    "Board Game Explorer",)
               ),
#### UI: Explore ####   
  tabsetPanel(type = "tabs",
              tabPanel("Explore Board Games",
                       h1("Overview"),
                       div(class = "table-class",
                         fluidRow(
                           column(4,
                                  img(src="https://images.unsplash.com/photo-1577897113292-3b95936e5206?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1090&q=80", 
                                      width="300px", 
                                      class = "center")
                                  ),
                           column(8,
                                  p("This is my short intro about boardgames in general. 
                           Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
                           sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
                           Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris 
                           nisi ut aliquip ex ea commodo consequat."),
                                  p(paste("The dataset used for this app consists of", 
                                          nrow(boardgames), 
                                          "boardgames from", 
                                          length(category_list),
                                          "different categories. 
                                 We also have information on their descriptions, age ratings, and user ratings, among others.")))
                         )
                       )

                       ,
                       br(),
                       
                       h3("Have a look at some of the games"),
                       div(class="table-class",
                           p("Tip: Sort the data using the arrows next to each column head and filter using the search bar."),
                           DT::dataTableOutput("preview_table"),
                           br(),
                           h3("Now let's see if we can find any trends in our data"),
                           br(),
                           h4("Boardgames have been on the rise in the past 50 years."),
                          
                           fluidRow(
                             column(6, 
                                    plotlyOutput("game_time")),
                             column(6,
                                    br(),
                                    br(),
                                    p(paste("The dataset contains boardgames up to thousands of years old. The oldest boardgame here is ",
                                            boardgames %>%
                                              filter(yearpublished == min(boardgames$yearpublished, na.rm = T)) %>% 
                                              select(primary) %>% unlist(), "which was published in the year",
                                            str_remove(min(boardgames$yearpublished, na.rm = T), "-"), 
                                            "B.C. Most games, however, were published in the last hundred years. 
                                   Notably, the year 1950 saw an exponential growth in the number of boardgames produced, 
                                   and the pattern continues today."))) 
                           ),
                           h4("Users gave generous ratings to the games"),
                           fluidRow(
                             column(6,
                                    plotlyOutput("game_ave")),
                             column(6,
                                    br(),
                                    br(),
                                    p(paste0("Users were generally generous with rating the games. 
                                    Put together, all boardgames have an average score of ",
                                             round(mean(boardgames$average, na.rm = T), 2),
                                             ". You might also notice that the graph resembles the bell-shaped normal distribution. 
                                          This is a natural consequence of the fact that our sample size is very large.")))
                           )

                          )  
                       
                       ),
#### UI: Compare ####
              tabPanel("Compare Games",
                       sidebarLayout(
                         sidebarPanel(
                           h4("Select 2 games to compare"),
                           tags$br(),
                           selectizeInput("game1",
                                          "Game #1",
                                          choices = c("No game selected", boardgames$primary),
                                          selected = "No game selected"),
                           selectizeInput("game2",
                                          "Game #2",
                                          choices = c("No game selected", boardgames$primary),
                                          selected = "No game selected"),
                           width = 2
                         ),
                         mainPanel(
                           fluidRow(
                             column(width = 5, offset = 1,
                                    h1(textOutput("game1")),
                                    fluidRow(
                                      column(2,
                                             tags$br(),
                                             uiOutput("img1")
                                             ),
                                      column(7,
                                             h3(textOutput("rank")),
                                             textOutput("rating"),
                                             ),
                                    ),
                                    tags$br(),
                                    uiOutput("table1"),
                                    tags$br(),

                                    uiOutput("description1")
                             ),
                             column(width = 5, offset = 1,
                                    h1(textOutput("game2")),
                                    fluidRow(
                                      column(2,
                                             tags$br(),
                                             uiOutput("img2")
                                      ),
                                      column(7,
                                             h3(textOutput("rank2")),
                                             textOutput("rating2"),
                                      ),
                                    ),
                                    tags$br(),
                                    uiOutput("table2"),
                                    tags$br(),
                                    
                                    uiOutput("description2")
                             )
                           )
                         )
                       )
                       ),
              tabPanel("Recommend me a Game",
                       h1("Recommend"))),
  
  
  
    
    
    )

    # Sidebar with a slider input for number of bins 
    

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#### Server: Explore ####
  
output$img_overview <- renderUI({
    tags$img(src = "https://images.unsplash.com/photo-1577897113292-3b95936e5206?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1090&q=80")
  })
  
output$preview_table <- renderDataTable({
  datatable(boardgames_preview, 
            options = list(scrollX = TRUE,
                           scrollY = TRUE,
                           pageLength = 3,
                           lengthChange = FALSE,
                           columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                             list(targets = c(0, 8, 9), visible = FALSE))))
  
  })

output$game_time <- renderPlotly(
  years_100 <- boardgames %>%
    filter(yearpublished > 1900) %>%
    ggplot(., aes(x = yearpublished)) +
    geom_bar() +
    labs(x = "Year",
         y = "Frequency",
         title = "Number of boardgames published since 1900") +
    scale_x_continuous(limits = c(1900, 2023))

)

output$game_ave <- renderPlotly(
  games_ave <- ggplot(boardgames, aes(x = average)) +
    geom_bar() +
    labs(x = "Frequency",
         y = "Average Rating",
         title = "Average Rating for Each Boardgame")
)
  

  
#### Server: Compare ####
	
  img1_url <- reactive({
    boardgames %>%
      filter(primary == input$game1) %>%
      select(thumbnail) %>% unlist(.)
  })
  
  
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
  
  desc1 <- reactive({
    desc <- boardgames %>%
      filter(primary == input$game1) %>%
      select(description) %>% unlist(.)
    
    desc <- substr(desc, 1, 500)
  })
  
  data1 <- reactive({
    boardgames %>%
      filter(primary == input$game1) %>%
      transmute(players = ifelse(minplayers == maxplayers, minplayers, paste0(minplayers, "-", maxplayers)),
                play_time = ifelse(minplaytime == maxplaytime, minplaytime, paste0(minplaytime, "-", maxplaytime)),
                rec_age = paste0(minage, "+")) %>%
      unlist(.)
  })
  
  # game 2
  
  img2_url <- reactive({
    boardgames %>%
      filter(primary == input$game2) %>%
      select(thumbnail) %>% unlist(.)
  })
  
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
  
  desc2 <- reactive({
    desc <- boardgames %>%
      filter(primary == input$game2) %>%
      select(description) %>% unlist(.)
    
    desc <- substr(desc, 1, 500)
  })
  
  data2 <- reactive({
    boardgames %>%
      filter(primary == input$game2) %>%
      transmute(players = ifelse(minplayers == maxplayers, minplayers, paste0(minplayers, "-", maxplayers)),
                play_time = ifelse(minplaytime == maxplaytime, minplaytime, paste(minplaytime, "-", maxplaytime)),
                rec_age = paste0(minage, "+")) %>%
      unlist(.)
  })
  
  #outputs
  
  output$logo <- renderUI({
    tags$img(src = "LOGO Main.png")
  })
  
  output$img1 <- renderUI({
    tags$img(src = img1_url())
  })
  
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
  
  output$description1 <- renderUI({
    if(input$game1 != "No game selected"){
      HTML(paste0(desc1(), "..."))
    }
  })
  
  output$table1 <- renderUI({
    if(input$game1 != "No game selected"){
      HTML(paste(data1()[1], "Players", "<br/>", "Playable in", data1()[2], "minutes<br/>", "Recommended for ages", data1()[3]))
    }
  })
  
  #game 2
  
  output$img2 <- renderUI({
    tags$img(src = img2_url())
  })
  
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
  
  output$description2 <- renderUI({
    if(input$game2 != "No game selected"){
      HTML(paste0(desc2(), "..."))
    }
  })
  
  output$table2 <- renderUI({
    if(input$game2 != "No game selected"){
      HTML(paste(data2()[1], "Players", "<br/>", "Playable in", data2()[2], "minutes<br/>", "Recommended for ages", data2()[3]))
    }
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
