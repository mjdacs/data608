library(tidyverse)
library(plotly)
library(shiny)


df <- read_csv("https://raw.githubusercontent.com/mjdacs/data608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv")
#df <- cleaned_cdc_mortality_1999_2010_2

ui <- fluidPage(
  headerPanel("Disease Rate Explorer"),
  sidebarPanel(
    selectInput('type', 'Disease Type', unique(df$ICD.Chapter)),
    selectInput('year', 'Year', unique(df$Year)),
    selectInput('state', 'State', unique(df$State))
  ),
  mainPanel(
    plotOutput('plot1'),
    plotOutput('plot2')
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    dfSlice <- df %>% 
      filter(ICD.Chapter == input$type, Year == input$year)  
      
    
    ggplot(dfSlice, aes(x = reorder(State, Crude.Rate), y = Crude.Rate)) +
      geom_bar(stat = "identity", fill = "navy", width = .5) +
      geom_text(aes(label=Crude.Rate),
                position=position_dodge(width = 0.1), hjust = -0.03, vjust = 0.5, size = 2.5) +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 8)) +
      labs(title = "Crude Disease Rate Per 100,000",
           subtitle = paste0(input$type," for the year ", input$year),
           caption = "mdac",
           x = "State", 
           y = "Rate")
  })
  
  output$plot2 <- renderPlot({
    
    state.rate <- df %>% 
      filter(ICD.Chapter==input$type, State==input$state)
    
    natl.rate <- df %>% 
      group_by(ICD.Chapter, Year) %>% 
      summarise(Natl.Avg=100000 / (mean(Population)/mean(Deaths))) %>% 
      filter(ICD.Chapter==input$type)
    
    ggplot() +
      geom_line(data = state.rate, aes(x = Year, y = Crude.Rate, color = "blue"), size = 2) +
      geom_line(data = natl.rate, aes(x = Year, y = Natl.Avg, color = "orange"), size = 2) +
      scale_color_discrete(name = "Averages", labels = c("National", paste0("State: ", input$state))) +
      labs(title = "State Disease Rate Compared to National Average",
           subtitle = paste0("Disease Type: ", input$type),
           caption = "mdac",
           x = "Year", 
           y = "Rate")
  })
  
}

shinyApp(ui, server)