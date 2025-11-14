# app.R
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

#File paths
#Iteration 1
rds_2018 <- "C:/Users/User/Documents/df2018.rds"
rds_2019 <- "C:/Users/User/Documents/df2019.rds"
rds_2020 <- "C:/Users/User/Documents/df2020.rds"
rds_2021 <- "C:/Users/User/Documents/df2021.rds"
rds_2022 <- "C:/Users/User/Documents/df2022.rds"
infokey  <- "C:/Users/User/Documents/Municipal_Finance_Data_Dictionary.csv"
#Iteration 2
rds_2018_neg <- "C:/Users/User/Documents/df2018_neg.rds"
rds_2019_neg <- "C:/Users/User/Documents/df2019_neg.rds"
rds_2020_neg <- "C:/Users/User/Documents/df2020_neg.rds"
rds_2021_neg <- "C:/Users/User/Documents/df2021_neg.rds"
rds_2022_neg <- "C:/Users/User/Documents/df2022_neg.rds"
infokey2  <- "C:/Users/User/Documents/Final_Table_Without_Original_Classification_Column (1).csv"

# Map years
rds_map <- list(
  "2018" = rds_2018,
  "2019" = rds_2019,
  "2020" = rds_2020,
  "2021" = rds_2021,
  "2022" = rds_2022
)

#quick load
dfs <- lapply(rds_map, readRDS)

# County list & numeric columns
all_counties <- sort(unique(unlist(lapply(dfs, \(d) as.character(d$CountyName)))))
num_cols <- Reduce(intersect, lapply(dfs, \(d) names(d)[sapply(d, is.numeric)]))


render_dt <- function(df) {
  datatable(df,
            options = list(pageLength = 10, scrollX = TRUE, searching = FALSE),
            rownames = FALSE)
}

ui <- fluidPage(
  titlePanel("iteration 1"),
  tabsetPanel(
    id = "tabs",
    
    tabPanel("Wisconsin Finance data by year",
             selectInput("year1", "Year:", choices = names(rds_map), selected = "2018"),
             DTOutput("table1")
    ),
    
    tabPanel("Wisconsin Finance Specified county",
             selectInput("year2", "Year:", choices = names(rds_map), selected = "2019"),
             selectInput("county2", "County:", choices = NULL),
             DTOutput("table2")
    ),
    
    tabPanel("Wisconsin Finance Column key", DTOutput("key")),
    
    tabPanel("Wisconsin Finance County totals per year",
             sidebarLayout(
               sidebarPanel(
                 selectInput("county4", "County:", choices = all_counties),
                 selectInput("col4", "Monetary column:", choices = num_cols)
               ),
               mainPanel(
                 plotOutput("line4"),
                 tableOutput("yearTbl4")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  #Tab1
  #quick output of data table
  output$table1 <- renderDT(render_dt(dfs[[input$year1]]))
  
  #Tab2
  #this selects the data set to display
  observeEvent(input$year2, {
    counties <- sort(unique(as.character(dfs[[input$year2]]$CountyName)))
    updateSelectInput(session, "county2", choices = counties,
                      selected = if (length(counties)) counties[1])
  }, ignoreInit = FALSE)
  
  #this renders the data set to display with the county
  output$table2 <- renderDT({
    req(input$year2, input$county2)
    d <- dfs[[input$year2]]
    d[d$CountyName == input$county2, , drop = FALSE]
  } |> render_dt())
  
  #Tab3
  #reads the file and prints it out
  key_df <- read.csv(infokey, check.names = FALSE)
  output$key <- renderDT(render_dt(key_df))
  
  #Tab4
  #reactivate resources
  year_totals <- reactive({
    sapply(names(dfs), function(y) {
      d <- dfs[[y]]
      v <- suppressWarnings(as.numeric(d[[input$col4]]))
      v[is.na(v)] <- 0
      #combine the municipalities
      sum(v[d$CountyName == input$county4], na.rm = TRUE)
    }) |> 
      (\(totals) data.frame(Year = as.integer(names(dfs)), Total = as.numeric(totals)))()
  })
  #render the line plot
  output$line4 <- renderPlot({
    yt <- year_totals()[order(year_totals()$Year), ]
    ggplot(yt, aes(Year, Total)) +
      geom_line() + geom_point(size = 2) +
      labs(title = paste(input$county4, "-", input$col4),
           x = "Year", y = "Total")
  })
  #render the data table
  output$yearTbl4 <- renderTable(year_totals()[order(year_totals()$Year), ])
}

shinyApp(ui, server)