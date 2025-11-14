library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(stringr)
library(DT)
library(grDevices)
library(ggplot2)
library(tidyr)
library(htmltools)

# ---- Map data ----
geojson_path <- "C:/Users/User/Documents/wisconsin-with-county-boundaries_1132 (1).geojson"
wi_counties <- st_read(geojson_path, quiet = TRUE) %>%
  mutate(county_name = name)

# ---- RDS paths ----
rds_paths_neg <- c(
  "2018" = "C:/Users/User/Documents/df2018_neg.rds",
  "2019" = "C:/Users/User/Documents/df2019_neg.rds",
  "2020" = "C:/Users/User/Documents/df2020_neg.rds",
  "2021" = "C:/Users/User/Documents/df2021_neg.rds",
  "2022" = "C:/Users/User/Documents/df2022_neg.rds"
)

# ---- helpers ----
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

# ============================================================
# UI
# ============================================================
ui <- navbarPage(
  title = "WI County Clusters by Financial Traits",
  
  # ---------------- Cluster Map ----------------
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
  
  # ---------------- County Table ----------------
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
  
  # ---------------- County Bar Charts ----------------
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
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ===================== MAP TAB =====================
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
  
  # ===================== TABLE TAB =====================
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
  
  # ===================== BAR CHARTS TAB =====================
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
}

shinyApp(ui, server)