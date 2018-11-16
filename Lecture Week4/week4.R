#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

pti <- c("shiny","reticulate","ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}

##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
  movies %>% 
  filter(year >= 2000) %>%
  select(title,year,length,rating,votes,Action:Short) %>% 
  gather(genre,value,Action:Short) %>% 
  filter(value == 1) %>% 
  select(-value)



# Get genre list
genres <- 
  shiny_movie_set %>% 
  distinct(genre) %>% 
  unlist(.)

names(genres) <- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Movie Lenght and IMDB Scores"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Years",
                     "Years:",
                     min = min(shiny_movie_set$year),
                     max = max(shiny_movie_set$year),
                     value = c(2002,2003), sep = ""),
         selectInput(inputId = "Genres",
                     label = "Genres",
                     choices = c("All",unique(shiny_movie_set$genre))),
         sliderInput(inputId = "votes",
                     label = "# of Votes",
                     min= min(shiny_movie_set$votes),
                     max= max(shiny_movie_set$votes),
                     value=0)
      
        ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("moviePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$moviePlot <- renderPlot({
    modified_data <- shiny_movie_set %>% filter(year>=input$Years[1] & year<=input$Years[2])
    modified_data <- modified_data %>% filter(votes>=input$votes) 
    if(input$Genres!="All") {
      modified_data <- modified_data %>% filter (genre==input$Genres)
      }
    ggplot(modified_data, aes(x=length, y=rating, color=genre)) + geom_point()
    
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

