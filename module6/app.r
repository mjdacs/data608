library(tidyverse)
library(shiny)
library(plotly)


df <- read_csv("http://web.mta.info/developers/data/nyct/turnstile/turnstile_191123.txt")

df <- df %>% filter(DIVISION %in% c("BMT", "IND", "IRT")) %>% 
  mutate(TIME = as.character(TIME),
         ENTRIES = as.integer(ENTRIES),
         EXITS = as.integer(EXITS))

df1 <- df %>%
  filter(
    (DATE == "11/16/2019" & TIME %in% c("00:00:00")) |
      (DATE == "11/22/2019" & TIME %in% c("20:00:00"))
  ) %>% 
  mutate(Total_Enter = ENTRIES - lag(ENTRIES, 1),
         Total_Exit = EXITS - lag(EXITS, 1)) %>% 
  filter(Total_Enter > 0 & DATE == "11/22/2019") %>% 
  group_by(STATION) %>% 
  filter(STATION != "ALLERTON AV" & !SCP %in% c("00-00-00"),
         STATION != "125 ST" & !SCP %in% c("00-05-01"),
         STATION != "WTC-CORTLANDT" & !SCP %in% c("00-03-00","03-00-00","03-00-01","03-00-02")) %>% 
  summarise(Week_Total_Entries = sum(Total_Enter),
            Week_Total_Exits = sum(Total_Exit)) %>% 
  arrange(desc(Week_Total_Entries))

df2 <- df %>%
  filter(
    (DATE == "11/16/2019" & TIME %in% c("01:00:00")) |
      (DATE == "11/22/2019" & TIME %in% c("21:00:00"))
  ) %>% 
  mutate(Total_Enter = ENTRIES - lag(ENTRIES, 1),
         Total_Exit = EXITS - lag(EXITS, 1)) %>% 
  filter(Total_Enter > 0 & DATE == "11/22/2019") %>% 
  group_by(STATION) %>% 
  summarise(Week_Total_Entries = sum(Total_Enter),
            Week_Total_Exits = sum(Total_Exit)) %>% 
  arrange(desc(Week_Total_Entries))

df3 <- df %>%
  filter(
    (DATE == "11/16/2019" & TIME %in% c("02:00:00")) |
      (DATE == "11/22/2019" & TIME %in% c("22:00:00"))
  ) %>% 
  mutate(Total_Enter = ENTRIES - lag(ENTRIES, 1),
         Total_Exit = EXITS - lag(EXITS, 1)) %>% 
  filter(Total_Enter > 0 & DATE == "11/22/2019") %>% 
  group_by(STATION) %>% 
  summarise(Week_Total_Entries = sum(Total_Enter),
            Week_Total_Exits = sum(Total_Exit)) %>% 
  arrange(desc(Week_Total_Entries))

df4 <- df %>%
  filter(
    (DATE == "11/16/2019" & TIME %in% c("03:00:00")) |
      (DATE == "11/22/2019" & TIME %in% c("23:00:00"))
  ) %>% 
  mutate(Total_Enter = ENTRIES - lag(ENTRIES, 1),
         Total_Exit = EXITS - lag(EXITS, 1)) %>% 
  filter(Total_Enter > 0 & DATE == "11/22/2019") %>% 
  group_by(STATION) %>%
  filter(STATION != "42 ST-PORT AUTH" & !SCP %in% c("00-00-08", "00-00-09"),
         STATION != "RECTOR ST" & !SCP %in% c("01-03-00", "01-06-00"),
         STATION != "47-50 STS ROCK" & !SCP %in% 
           c("02-06-00","02-06-01",
             "02-03-00","02-03-01","02-03-02","02-03-03",
             "01-03-02",
             "00-03-00", "00-03-01","00-03-02","00-03-03","00-03-04","00-03-05"),
         STATION != "86 ST" & !SCP %in% 
           c("00-00-00", "00-00-01", "00-03-00", "00-03-01", "00-03-02")
  ) %>%       
  summarise(Week_Total_Entries = sum(Total_Enter),
            Week_Total_Exits = sum(Total_Exit)) %>% 
  arrange(desc(Week_Total_Entries))

clean_df <- bind_rows(df1, df2, df3, df4)

total.by.station <- clean_df %>% group_by(STATION) %>% 
  summarise(Week_Total_Entries = sum(Week_Total_Entries),
            Week_Total_Exits = sum(Week_Total_Exits)) %>% 
  arrange(desc(Week_Total_Entries))

data_viz_hi <- head(total.by.station,20) %>% 
  mutate(Total_Traffic = Week_Total_Entries + Week_Total_Exits) %>% 
  select(STATION,Total_Traffic) %>% 
  arrange(desc(Total_Traffic))


data_viz_low <- tail(total.by.station,20) %>% 
  mutate(Total_Traffic = Week_Total_Entries + Week_Total_Exits) %>% 
  select(STATION, Total_Traffic) %>% 
  arrange(Total_Traffic)

graph.it <- function(df, col_order, title_phrase) {
  ggplot(df, aes(x = reorder(STATION, col_order), y = Total_Traffic)) +
    geom_bar(stat = "identity", fill = "navy", width = .5) +
    geom_text(aes(label = Total_Traffic),
              position = position_dodge(width = 0.1), hjust = -0.03, vjust = 0.5, size = 2.5) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 10)) +
    labs(title = paste("20", title_phrase, "Stations for Week Ending 11/22/2019"),
         subtitle = "Total turnstile entries and exits. Data courtesty of mta.info",
         caption = "mdac",
         x = "Station",
         y = "Entries per station") 
  
}


ui <- fluidPage(
  headerPanel("NYC Subway Turnstile Info"),
  sidebarPanel(
    selectInput('station', 'All NYC Stations', c('Most Frequented', 'Least Frequented')),
    selectInput('borough', 'Borough', c('Manhattan', 'Brooklyn', 'Bronx', 'Queens')),
    selectInput('borough-metric', 'Borough Frequency', c('Top 20', 'Bottom 20'))
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    

    if (input$station == "Most Frequented") {
      graph.it(data_viz_hi, data_viz_hi$Total_Traffic, "Busiest")
    } else {
      graph.it(data_viz_low, -data_viz_low$Total_Traffic, "Least Busy")
    }
    
    
    
  })
}

shinyApp(ui, server)