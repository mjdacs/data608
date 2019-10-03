library(tidyverse)
library(plotly)
library(shiny)

#df <- read_csv("https://raw.githubusercontent.com/mjdacs/data608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv")
df <- cleaned_cdc_mortality_1999_2010_2

ui <- fluidPage(
  headerPanel("Disease Rate Explorer"),
  sidebarPanel(
    selectInput('type', 'Disease Type', unique(df$ICD.Chapter)),
    selectInput('year', 'Year', unique(df$Year)),
    selectInput('state', 'State', unique(df$State))
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    dfSlice <- df %>% 
      filter(ICD.Chapter == input$type, Year == input$year)  
      
    
    ggplot(dfSlice, aes(x = reorder(State, Crude.Rate), y = Crude.Rate)) +
      geom_bar(stat = "identity") +
      coord_flip()
  })
}

shinyApp(ui, server)