library(shiny)
library(tidyverse)
library(shinythemes)
library(RCurl)

bcl <- read_csv("bcl-data.csv")

ggplot(bcl, aes(Alcohol_Content)) +
  geom_histogram()

ui <- fluidPage(theme = shinytheme("united"), #Added a theme to make it look nicer
  titlePanel("BC Liquor Store Data"),
  h5("Welcome to my shiny app!"),
  br(), #line break
  
  sidebarLayout(sidebarPanel(
    sliderInput("priceInput", "Price", 0, 100, 
                value=c(25, 40), pre = "$"),
    radioButtons("typeInput", "Type", 
                 choices = c("BEER", "REFRESHMENTS", "SPIRITS", "WINE"))
  ), 
  
  mainPanel(
    tabsetPanel( #Added this argument to place the plot and table on separate panels, easier to read
      tabPanel("Plot", plotOutput("alcohol_hist")),
      tabPanel("Table", DT::dataTableOutput("data_table")) #Making the data table interactive
    )
  )),
  
  a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", 
    "Link to the original dataset")
)

server <- function(input, output) {
  
  filtered_data <- 
    reactive(
      bcl %>% filter(Price > input$priceInput[1] &
                       Price < input$priceInput[2] &
                       Type == input$typeInput)
    )
  
  output$alcohol_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Alcohol_Content)) + 
        geom_histogram()
    })
  output$data_table <-
      DT::renderDataTable( #making the data table interactive
        filtered_data()
      )
}

shinyApp(ui = ui, server = server)