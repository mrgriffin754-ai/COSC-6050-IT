# app.R
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(viridisLite)
library(DT)
library(stringr)
library(grDevices)
library(tidyr)
library(htmltools)
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
# Geo map
geojson_path <- "C:/Users/User/Documents/wisconsin-with-county-boundaries_1132 (1).geojson"
wi_counties <- st_read(geojson_path, quiet = TRUE) %>%
  mutate(county_name = name)
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
#Iteration 2
# Label points safely
wi_proj    <- st_transform(wi_counties, 3071)
centers_p  <- st_point_on_surface(wi_proj)
centers    <- st_transform(centers_p, 4326)
ctr_xy     <- st_coordinates(centers) %>% as.data.frame()
centers    <- centers %>% mutate(x = ctr_xy$X, y = ctr_xy$Y)
#RDS paths
rds_paths_neg <- c(
  "2018" = "C:/Users/User/Documents/df2018_neg.rds",
  "2019" = "C:/Users/User/Documents/df2019_neg.rds",
  "2020" = "C:/Users/User/Documents/df2020_neg.rds",
  "2021" = "C:/Users/User/Documents/df2021_neg.rds",
  "2022" = "C:/Users/User/Documents/df2022_neg.rds"
)

# itteration 3
#RDS paths
rds_paths_neg <- c(
  "2018" = "C:/Users/User/Documents/df2018_neg.rds",
  "2019" = "C:/Users/User/Documents/df2019_neg.rds",
  "2020" = "C:/Users/User/Documents/df2020_neg.rds",
  "2021" = "C:/Users/User/Documents/df2021_neg.rds",
  "2022" = "C:/Users/User/Documents/df2022_neg.rds"
)

#helpers
get_county_col <- function(df) {
  nms <- tolower(names(df))
  hit <- names(df)[nms %in% c("county", "countyname", "county_name")]
  if (length(hit) > 0) hit[1] else if ("CountyName" %in% names(df)) "CountyName" else NULL
}

aggregate_to_county <- function(df) {
  ccol <- get_county_col(df)
  if (is.null(ccol)) return(df)
  df <- df %>% rename(County = all_of(ccol))
  num_cols <- names(df)[sapply(df, is.numeric)]
  df %>%
    group_by(County) %>%
    summarise(across(all_of(num_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
}

scale_safe <- function(m) {
  cen <- colMeans(m)
  s <- apply(m, 2, sd)
  s[!is.finite(s) | s == 0] <- 1
  sweep(sweep(m, 2, cen, "-"), 2, s, "/")
}

normalize_key <- function(x) {
  x <- tolower(x)
  x <- gsub("&", " and ", x)
  x <- gsub("\\bst\\.?\\b", "saint", x)
  x <- gsub("'", "", x)
  x <- gsub("\\s+county$", "", x)
  x <- gsub("[^a-z0-9 ]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
#Itteration 4
df2018 <- readRDS("C:/Users/User/Documents/df2018_county.rds")
df2019 <- readRDS("C:/Users/User/Documents/df2019_county.rds")
df2020 <- readRDS("C:/Users/User/Documents/df2020_county.rds")
df2021 <- readRDS("C:/Users/User/Documents/df2021_county.rds")
df2022 <- readRDS("C:/Users/User/Documents/df2022_county.rds")

# Standardize columns to match 2022
common_cols <- names(df2022)
df2018 <- df2018[, common_cols]
df2019 <- df2019[, common_cols]
df2020 <- df2020[, common_cols]
df2021 <- df2021[, common_cols]
df2022 <- df2022[, common_cols]

# List of all years for time-series stuff
all_years <- list(
  "2018" = df2018,
  "2019" = df2019,
  "2020" = df2020,
  "2021" = df2021,
  "2022" = df2022
)

# County choices and numeric column selection
county_choices <- sort(unique(df2022$CountyName))
numeric_cols <- names(df2022)[sapply(df2022, is.numeric)]
numeric_cols <- setdiff(numeric_cols, "CountyCode")
numeric_cols <- setdiff(numeric_cols, "MuniCode")
numeric_cols <- setdiff(numeric_cols, c("Year"))

# Helper: handles gaps for field-based trend
build_time_series <- function(county, field, datasets) {
  years <- as.numeric(names(datasets))
  
  df <- lapply(seq_along(datasets), function(i) {
    yr  <- years[i]
    dat <- datasets[[i]]
    
    row <- dat[dat$CountyName == county, , drop = FALSE]
    
    if (nrow(row) == 0) {
      value <- NA_real_
    } else {
      value <- suppressWarnings(as.numeric(row[[field]][1]))
    }
    
    data.frame(
      year  = yr,
      value = value
    )
  }) %>% bind_rows()
  
  # remove missing values
  df <- df %>% filter(!is.na(value))
}

# Helper: compute yearly county totals (sum of all fields after Population)
compute_year_totals <- function(df) {
  pop_index <- which(names(df) == "Population")
  monetary_cols <- names(df)[(pop_index + 1):ncol(df)]
  
  df %>%
    mutate(TotalValue = rowSums(across(all_of(monetary_cols)), na.rm = TRUE)) %>%
    select(CountyName, Population, TotalValue)
}

# Compute totals per year
tot2018 <- compute_year_totals(df2018)
tot2019 <- compute_year_totals(df2019)
tot2020 <- compute_year_totals(df2020)
tot2021 <- compute_year_totals(df2021)
tot2022 <- compute_year_totals(df2022)

# Long data frame of county totals by year (for county income projection)
county_year_totals <- bind_rows(
  transform(tot2018, Year = 2018),
  transform(tot2019, Year = 2019),
  transform(tot2020, Year = 2020),
  transform(tot2021, Year = 2021),
  transform(tot2022, Year = 2022)
)

# Sum of totals across all counties per year (for overall yearly projection)
year_totals <- county_year_totals %>%
  group_by(Year) %>%
  summarise(TotalValue = sum(TotalValue), .groups = "drop")


#UI==========================================================================================================================================
ui <- fluidPage(
  titlePanel("Wisconsin finance app (iteration 1-4)"),
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
    ),
    tabPanel("Map of wisconsin", leafletOutput("map_unique", height = 680)),
    tabPanel("County monitary change table",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year_neg", "Select Year", choices = names(rds_paths_neg), selected = "2022"),
                 uiOutput("col_adjusters"),   # <-- dynamic per-column % sliders
                 downloadButton("download_agg", "Download County Totals (CSV)")
               ),
               mainPanel(DTOutput("table_agg"))
             )
    ),
    tabPanel("Combined monitary Totals(change table)",
             sidebarLayout(
               sidebarPanel(
                 helpText("Combines all Monitary columns."),
                 downloadButton("download_combined", "Download Combined Totals (CSV)")
               ),
               mainPanel(DTOutput("table_combined"))
             )
    ),
    tabPanel("Standardized z-score Table(change table)",
             sidebarLayout(
               sidebarPanel(
                 helpText("Z-score standardization of Combined Total (mean = 0, sd = 1)."),
                 downloadButton("download_standardized", "Download Standardized Table (CSV)")
               ),
               mainPanel(DTOutput("table_standardized"))
             )
    ),
    tabPanel("Heat Map(change table)",
             sidebarLayout(
               sidebarPanel(
                 helpText("Heat map colored by Z_Score ")
               ),
               mainPanel(leafletOutput("map_zscore", height = 680))
             )
    ),
    # Cluster Map
    tabPanel(
      "Cluster Map",
      sidebarLayout(
        sidebarPanel(
          h4("Map / Clustering controls"),
          selectInput("map_year", "Year (map/cluster)", choices = names(rds_paths_neg), selected = "2022"),
          checkboxInput("map_standardize", "Standardize selected variables for k-means", value = TRUE),
          uiOutput("map_vars_ui"),
          sliderInput("map_k", "Clusters (k)", min = 2, max = 8, value = 3),
          actionButton("map_run", "Run K-means", class = "btn-primary")
        ),
        mainPanel(leafletOutput("cluster_map", height = 700))
      )
    ),
    
    # County Table
    tabPanel(
      "County Table",
      sidebarLayout(
        sidebarPanel(
          h4("Table controls"),
          selectInput("tbl_year", "Year (table clustering)", choices = names(rds_paths_neg), selected = "2022"),
          checkboxInput("tbl_standardize", "Standardize selected variables for k-means", value = TRUE),
          uiOutput("tbl_vars_ui"),
          sliderInput("tbl_k", "Clusters (k)", min = 2, max = 8, value = 3),
          actionButton("tbl_run", "Run K-means (table)", class = "btn-primary"),
          hr(),
          downloadButton("download_table", "Download table (CSV)")
        ),
        mainPanel(DTOutput("cluster_table"))
      )
    ),
    
    # County Bar Charts
    tabPanel(
      "County Bar Charts",
      sidebarLayout(
        sidebarPanel(
          h4("Bar chart controls"),
          selectInput("bar_year", "Year (bar charts)", choices = names(rds_paths_neg), selected = "2022"),
          uiOutput("county_picker_1"),
          uiOutput("county_picker_2"),
          uiOutput("bar_var_picker"),
          helpText("Bar charts use aggregated values from the selected bar chart year, independent of the other tabs.")
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("county_bar_1", height = 520)),
            column(6, plotOutput("county_bar_2", height = 520))
          )
        )
      )
    ),
    tabPanel(
      "Trend & Prediction",
      sidebarLayout(
        sidebarPanel(
          selectInput("pred_county", "Select County:", choices = county_choices),
          selectInput("pred_field", "Select Field:", choices = numeric_cols),
          helpText("Linear regression uses all non-missing values available for that county and field.")
        ),
        mainPanel(
          plotOutput("trend_plot", height = "350px"),
          br(),
          tableOutput("prediction_table")
        )
      )
    ),
    
    # County Income Projection
    tabPanel(
      "County Income Projection",
      sidebarLayout(
        sidebarPanel(
          selectInput("income_county", "Select County:", choices = county_choices),
          helpText("Combined income = sum of all monetary fields after Population for that county, per year.")
        ),
        mainPanel(
          plotOutput("county_income_plot", height = "350px"),
          br(),
          tableOutput("county_income_table")
        )
      )
    ),
    
    #Yearly projection
    tabPanel(
      "Yearly Projection",
      plotOutput("proj_plot", height = "350px"),
      br(),
      tableOutput("proj_table")
    )
  )
)
#server=======================================================================================================================================
server <- function(input, output, session) {
#itteration 1==========================================================================================================================================
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
  year_totals1 <- reactive({
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
    yt <- year_totals1()[order(year_totals1()$Year), ]
    ggplot(yt, aes(Year, Total)) +
      geom_line() + geom_point(size = 2) +
      labs(title = paste(input$county4, "-", input$col4),
           x = "Year", y = "Total")
  })
  #render the data table
  output$yearTbl4 <- renderTable(year_totals1()[order(year_totals1()$Year), ])
  
#iteration 3===============================================================================================================================
  # Map (unique colors)
  output$map_unique <- renderLeaflet({
    cats <- sort(unique(wi_counties$county_name))
    pal_vec <- hcl.colors(length(cats), palette = "Set3")
    pal <- colorFactor(palette = pal_vec, domain = cats)
    leaflet(wi_counties) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(county_name), color = "black", weight = 0.5,
        fillOpacity = 0.7, label = ~county_name,
        highlightOptions = highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
      ) |>
      addLabelOnlyMarkers(
        data = centers, lng = ~x, lat = ~y, label = ~county_name,
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textsize = "10px")
      )
  })
  
  #Load selected dataset
  neg_df <- reactive({
    readRDS(rds_paths_neg[input$year_neg])
  })
  
  # Numeric columns (excluding Population, CountyCode, Year, MuniCode)
  numeric_cols <- reactive({
    df <- as.data.frame(neg_df())
    exclude_cols <- c("MuniCode", "Population", "CountyCode", "Year", "year", "countycode")
    df <- df[, setdiff(colnames(df), exclude_cols), drop = FALSE]
    cols <- names(df)[sapply(df, is.numeric)]
    cols
  })
  
  # Build sliders for numeric columns
  output$col_adjusters <- renderUI({
    req(numeric_cols())
    lapply(numeric_cols(), function(col) {
      sliderInput(
        inputId = paste0("pct_", col),
        label   = paste0(col, " (% change)"),
        min = -100, max = 100, value = 0, step = 1
      )
    })
  })
  
  # Apply adjustments
  adjusted_df <- reactive({
    df <- as.data.frame(neg_df())
    # Remove excluded fields
    df <- dplyr::select(df, -any_of(c("MuniCode", "CountyCode", "Year", "year")))
    cols <- numeric_cols()
    for (col in cols) {
      pct_input_id <- paste0("pct_", col)
      pct <- input[[pct_input_id]]
      if (is.null(pct)) pct <- 0
      df[[col]] <- df[[col]] * (1 + pct / 100)
    }
    df
  })
  
  # Aggregate to County level
  county_agg <- reactive({
    df <- adjusted_df()
    df %>%
      group_by(CountyName) %>%
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      arrange(CountyName)
  })
  
  output$table_agg <- renderDT({
    datatable(county_agg(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$download_agg <- downloadHandler(
    filename = function() paste0("county_totals_", input$year_neg, "_neg.csv"),
    content  = function(file) write.csv(county_agg(), file, row.names = FALSE)
  )
  
  # Combined Totals
  combined_totals <- reactive({
    df <- county_agg()
    pop_idx <- which(colnames(df) == "Population")
    cols_after_pop <- colnames(df)[(pop_idx + 1):ncol(df)]
    cols_after_pop <- cols_after_pop[sapply(df[cols_after_pop], is.numeric)]
    df %>%
      mutate(Combined_Total = rowSums(dplyr::select(., dplyr::all_of(cols_after_pop)), na.rm = TRUE)) %>%
      dplyr::select(CountyName, Population, Combined_Total)
  })
  
  output$table_combined <- renderDT({
    datatable(combined_totals(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  #Standardized Table
  standardized_table <- reactive({
    df <- combined_totals()
    m  <- mean(df$Combined_Total, na.rm = TRUE)
    s  <- sd(df$Combined_Total,   na.rm = TRUE)
    if (is.na(s) || s == 0) df$Z_Score <- 0 else df$Z_Score <- (df$Combined_Total - m) / s
    df %>% dplyr::select(-Combined_Total) %>% arrange(desc(Z_Score))
  })
  
  output$table_standardized <- renderDT({
    datatable(standardized_table(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Heat Map (Z-Score)
  output$map_zscore <- renderLeaflet({
    std <- standardized_table()
    std <- std %>% mutate(CountyName = str_trim(str_to_upper(CountyName)))
    shapes <- wi_counties %>% mutate(county_name = str_trim(str_to_upper(county_name)))
    joined <- left_join(shapes, std, by = c("county_name" = "CountyName"))
    
    z <- joined$Z_Score
    if (all(is.na(z))) z <- rep(0, nrow(joined))
    maxabs <- suppressWarnings(max(abs(z), na.rm = TRUE))
    if (!is.finite(maxabs) || maxabs == 0) maxabs <- 0.1
    center_point <- -.5
    domain_min <- min(z, na.rm = TRUE)
    domain_max <- max(z, na.rm = TRUE)
    spread <- max(domain_max - center_point, center_point - domain_min)
    domain_range <- c(center_point - spread, center_point + spread)
    
    pal <- colorNumeric(
      palette = colorRampPalette(hcl.colors(11, "Blue-Red 3"))(255),
      domain  = domain_range,
      na.color = "#f0f0f0"
    )
    
    leaflet(joined) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(Z_Score),
        color = "#333333", weight = 0.7,
        fillOpacity = 0.85,
        label = lapply(
          sprintf(
            "<b>%s</b><br/>Population: %s<br/>Z-Score: %0.2f ",
            joined$county_name,
            ifelse(is.na(joined$Population), "-", formatC(joined$Population, big.mark = ",")),
            joined$Z_Score
          ),
          htmltools::HTML
        ),
        highlightOptions = highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Z_Score,
        title = "Z-Score",
        opacity = 1
      )
  })
#Iteration2===============================================================================================================================
  raw_df_map <- reactive({
    path <- rds_paths_neg[[input$map_year]]
    if (file.exists(path)) readRDS(path) else NULL
  })
  
  county_df_map <- reactive({
    df <- raw_df_map()
    if (is.null(df)) return(NULL)
    aggregate_to_county(df)
  })
  
  output$map_vars_ui <- renderUI({
    agg <- county_df_map()
    if (is.null(agg)) return(NULL)
    num <- names(agg)[sapply(agg, is.numeric)]
    num <- setdiff(num, c("CountyCode", "FIPS", "GEOID", "Year", "year"))
    checkboxGroupInput("map_vars", "Numeric variables (for k-means):", choices = num, selected = num)
  })
  
  prepared_map <- reactive({
    agg <- county_df_map()
    if (is.null(agg) || is.null(input$map_vars)) return(NULL)
    X <- as.matrix(agg[, input$map_vars, drop = FALSE])
    X[!is.finite(X)] <- NA
    keep <- complete.cases(X)
    X <- X[keep, , drop = FALSE]
    agg <- agg[keep, , drop = FALSE]
    sds <- apply(X, 2, sd, na.rm = TRUE)
    ok <- is.finite(sds) & sds > 0
    X <- X[, ok, drop = FALSE]
    Xs <- if (isTRUE(input$map_standardize)) scale_safe(X) else X
    list(Xs = Xs, df = agg)
  })
  
  observe({
    p <- prepared_map()
    if (is.null(p)) return()
    nmax <- max(2, min(nrow(unique(p$Xs)), 20))
    updateSliderInput(session, "map_k", max = nmax)
  })
  
  km_tbl_map <- eventReactive(input$map_run, {
    p <- prepared_map()
    if (is.null(p) || nrow(p$Xs) < 2) return(NULL)
    k <- max(2, min(input$map_k, nrow(unique(p$Xs))))
    set.seed(42)
    km <- kmeans(p$Xs, centers = k, nstart = 25)
    out <- p$df %>% mutate(Cluster = km$cluster) %>% relocate(Cluster, .after = County)
    list(table = out, km = km)
  })
  
  output$cluster_map <- renderLeaflet({
    res <- km_tbl_map()
    if (is.null(res)) {
      return(
        leaflet(wi_counties) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(color = "black", fillColor = "#d9d9d9", weight = 0.5,
                      fillOpacity = 0.6, label = ~county_name)
      )
    }
    
    tab <- res$table
    shapes_norm <- wi_counties %>% mutate(join_key = normalize_key(county_name))
    tab_norm <- tab %>% mutate(join_key = normalize_key(County)) %>% select(join_key, Cluster)
    joined <- left_join(shapes_norm, tab_norm, by = "join_key")
    
    cl_vals <- sort(unique(joined$Cluster))
    cl_vals <- cl_vals[is.finite(cl_vals)]
    if (length(cl_vals) < 1) cl_vals <- 1L
    pal <- colorFactor(hcl.colors(max(cl_vals), "Set3"), domain = cl_vals, na.color = "#dddddd")
    
    leaflet(joined) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(Cluster),
        color = "#333", weight = 0.7,
        fillOpacity = 0.85,
        label = lapply(
          sprintf("<b>%s</b><br/>Cluster: %s",
                  joined$county_name,
                  ifelse(is.na(joined$Cluster), "NA", as.character(joined$Cluster))),
          HTML
        )
      ) |>
      addLegend("bottomright", pal = pal, values = ~Cluster, title = "Cluster")
  })
  
  #TABLE TAB
  raw_df_tbl <- reactive({
    path <- rds_paths_neg[[input$tbl_year]]
    if (file.exists(path)) readRDS(path) else NULL
  })
  
  county_df_tbl <- reactive({
    df <- raw_df_tbl()
    if (is.null(df)) return(NULL)
    aggregate_to_county(df)
  })
  
  output$tbl_vars_ui <- renderUI({
    agg <- county_df_tbl()
    if (is.null(agg)) return(NULL)
    num <- names(agg)[sapply(agg, is.numeric)]
    num <- setdiff(num, c("CountyCode", "FIPS", "GEOID", "Year", "year"))
    checkboxGroupInput("tbl_vars", "Numeric variables (for k-means and table):", choices = num, selected = num)
  })
  
  prepared_tbl <- reactive({
    agg <- county_df_tbl()
    if (is.null(agg) || is.null(input$tbl_vars)) return(NULL)
    X <- as.matrix(agg[, input$tbl_vars, drop = FALSE])
    X[!is.finite(X)] <- NA
    keep <- complete.cases(X)
    X <- X[keep, , drop = FALSE]
    agg <- agg[keep, , drop = FALSE]
    sds <- apply(X, 2, sd, na.rm = TRUE)
    ok <- is.finite(sds) & sds > 0
    X <- X[, ok, drop = FALSE]
    Xs <- if (isTRUE(input$tbl_standardize)) scale_safe(X) else X
    list(Xs = Xs, df = agg)
  })
  
  observe({
    p <- prepared_tbl()
    if (is.null(p)) return()
    nmax <- max(2, min(nrow(unique(p$Xs)), 20))
    updateSliderInput(session, "tbl_k", max = nmax)
  })
  
  km_tbl_tbl <- eventReactive(input$tbl_run, {
    p <- prepared_tbl()
    if (is.null(p) || nrow(p$Xs) < 2) return(NULL)
    k <- max(2, min(input$tbl_k, nrow(unique(p$Xs))))
    set.seed(42)
    km <- kmeans(p$Xs, centers = k, nstart = 25)
    out <- p$df %>% mutate(Cluster = km$cluster) %>% relocate(Cluster, .after = County)
    list(table = out, km = km)
  })
  
  output$cluster_table <- renderDT({
    res <- km_tbl_tbl()
    if (is.null(res)) return(NULL)
    datatable(res$table, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_table <- downloadHandler(
    filename = function() paste0("county_clusters_", input$tbl_year, ".csv"),
    content = function(file) {
      res <- km_tbl_tbl()
      if (is.null(res)) return(NULL)
      write.csv(res$table, file, row.names = FALSE)
    }
  )
  
  #BAR CHARTS TAB
  raw_df_bar <- reactive({
    path <- rds_paths_neg[[input$bar_year]]
    if (file.exists(path)) readRDS(path) else NULL
  })
  
  county_df_bar <- reactive({
    df <- raw_df_bar()
    if (is.null(df)) return(NULL)
    aggregate_to_county(df)
  })
  
  output$county_picker_1 <- renderUI({
    agg <- county_df_bar()
    if (is.null(agg)) return(NULL)
    selectInput("bar_county_1", "Select County A", choices = sort(unique(agg$County)))
  })
  
  output$county_picker_2 <- renderUI({
    agg <- county_df_bar()
    if (is.null(agg)) return(NULL)
    choices <- sort(unique(agg$County))
    default_b <- if (!is.null(input$bar_county_1)) setdiff(choices, input$bar_county_1)[1] else choices[2]
    selectInput("bar_county_2", "Select County B", choices = choices, selected = default_b)
  })
  
  output$bar_var_picker <- renderUI({
    agg <- county_df_bar()
    if (is.null(agg)) return(NULL)
    num <- names(agg)[sapply(agg, is.numeric)]
    num <- setdiff(num, c("CountyCode", "FIPS", "GEOID", "Year", "year"))
    checkboxGroupInput("bar_vars", "Columns to plot (aggregated values):",
                       choices = num, selected = head(num, 6))
  })
  
  make_county_bar <- function(agg, county, cols, year_label) {
    if (is.null(county) || is.null(cols) || length(cols) == 0) return(NULL)
    row <- agg %>% filter(County == county)
    if (nrow(row) == 0) return(NULL)
    cols <- intersect(cols, names(row))
    cols <- cols[sapply(row[cols], is.numeric)]
    if (length(cols) == 0) return(NULL)
    long <- row %>% select(all_of(cols)) %>% pivot_longer(everything(),
                                                          names_to = "Variable", values_to = "Value")
    ggplot(long, aes(x = Variable, y = Value, fill = Variable)) +
      geom_col() +
      scale_fill_viridis_d(option = "plasma") +
      coord_flip() +
      labs(title = paste("Aggregated values -", year_label, "-", county),
           x = "Variable", y = "Value") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  }
  
  output$county_bar_1 <- renderPlot({
    agg <- county_df_bar()
    if (is.null(agg)) return(invisible(NULL))
    make_county_bar(agg, input$bar_county_1, input$bar_vars, input$bar_year)
  })
  
  output$county_bar_2 <- renderPlot({
    agg <- county_df_bar()
    if (is.null(agg)) return(invisible(NULL))
    make_county_bar(agg, input$bar_county_2, input$bar_vars, input$bar_year)
  })
#Iteration4================================================================================================================================
 ts_data <- reactive({
    build_time_series(
      county   = input$pred_county,
      field    = input$pred_field,
      datasets = all_years
    )
  })
  
  pred_data <- reactive({
    df <- ts_data()
    
    fit <- lm(value ~ year, data = df)
    max_year <- max(df$year)
    
    future <- data.frame(year = (max_year + 1):(max_year + 5))
    future$pred <- predict(fit, newdata = future)
    
    list(
      observed = df,
      future   = future
    )
  })
  
  output$trend_plot <- renderPlot({
    pd <- pred_data()
    df_obs <- pd$observed
    df_future <- pd$future
    
    df_obs$Type <- "Observed"
    df_future$Type <- "Predicted"
    df_future$value <- df_future$pred
    
    plot_df <- bind_rows(df_obs, df_future)
    
    ggplot(plot_df, aes(x = year, y = value, linetype = Type, shape = Type)) +
      geom_line() +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        title = paste("Trend & Prediction for", input$pred_field, "in", input$pred_county),
        x = "Year", y = input$pred_field
      )
  })
  
  output$prediction_table <- renderTable({
    pd <- pred_data()
    
    observed <- pd$observed %>%
      mutate(Type = "Observed") %>%
      select(Year = year, Type, Value = value)
    
    future <- pd$future %>%
      mutate(Type = "Predicted") %>%
      select(Year = year, Type, Value = pred)
    
    bind_rows(observed, future)
  })
  
  # ---- County Income Projection (TotalValue per county) ----
  county_income_data <- reactive({
    county_year_totals %>%
      filter(CountyName == input$income_county) %>%
      arrange(Year)
  })
  
  county_income_pred <- reactive({
    df <- county_income_data()
    
    fit <- lm(TotalValue ~ Year, data = df)
    max_year <- max(df$Year)
    
    future <- data.frame(Year = (max_year + 1):(max_year + 5))
    future$TotalValue <- predict(fit, newdata = future)
    future$Type <- "Predicted"
    
    observed <- df %>%
      transmute(Year, TotalValue, Type = "Observed")
    
    bind_rows(observed, future)
  })
  
  output$county_income_plot <- renderPlot({
    df <- county_income_pred()
    
    ggplot(df, aes(x = Year, y = TotalValue, color = Type, shape = Type)) +
      geom_line() +
      geom_point(size = 3) +
      theme_minimal() +
      labs(
        title = paste("Combined Income Projection for", input$income_county),
        x = "Year",
        y = "Total Combined Income (All Monetary Fields)"
      )
  })
  
  output$county_income_table <- renderTable({
    county_income_pred()
  })
  
  # ---- Yearly projection (all counties combined) ----
  proj_data <- reactive({
    fit <- lm(TotalValue ~ Year, data = year_totals)
    
    future <- data.frame(Year = 2023:2027)
    future$TotalValue <- predict(fit, newdata = future)
    future$Type <- "Predicted"
    
    observed <- year_totals %>% mutate(Type = "Observed")
    
    bind_rows(observed, future)
  })
  
  output$proj_plot <- renderPlot({
    fp <- proj_data()
    
    ggplot(fp, aes(x = Year, y = TotalValue, color = Type, shape = Type)) +
      geom_line() +
      geom_point(size = 3) +
      theme_minimal() +
      labs(
        title = "Total Monetary Value (All Counties): 5-Year Projection",
        x = "Year", y = "Total Value"
      )
  })
  
  output$proj_table <- renderTable({
    proj_data()
  })
}

shinyApp(ui, server)