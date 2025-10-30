# app.R
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(viridisLite)
library(DT)
library(stringr)
library(grDevices)
# ---- Map data (already working) ----
geojson_path <- "C:/Users/User/Documents/wisconsin-with-county-boundaries_1132 (1).geojson"
wi_counties <- st_read(geojson_path, quiet = TRUE) %>%
  mutate(county_name = name)

# Label points safely
wi_proj    <- st_transform(wi_counties, 3071)
centers_p  <- st_point_on_surface(wi_proj)
centers    <- st_transform(centers_p, 4326)
ctr_xy     <- st_coordinates(centers) %>% as.data.frame()
centers    <- centers %>% mutate(x = ctr_xy$X, y = ctr_xy$Y)

# ---- RDS paths ----
rds_paths_neg <- c(
  "2018" = "C:/Users/User/Documents/df2018_neg.rds",
  "2019" = "C:/Users/User/Documents/df2019_neg.rds",
  "2020" = "C:/Users/User/Documents/df2020_neg.rds",
  "2021" = "C:/Users/User/Documents/df2021_neg.rds",
  "2022" = "C:/Users/User/Documents/df2022_neg.rds"
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Wisconsin Counties — Aggregated, Standardized, and Mapped"),
  tabsetPanel(
    tabPanel("Map of wisconsin", leafletOutput("map_unique", height = 680)),
    tabPanel("County simplified",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year_neg", "Select Year", choices = names(rds_paths_neg), selected = "2022"),
                 uiOutput("col_adjusters"),   # <-- dynamic per-column % sliders
                 downloadButton("download_agg", "Download County Totals (CSV)")
               ),
               mainPanel(DTOutput("table_agg"))
             )
    ),
    tabPanel("Combined monitary Totals",
             sidebarLayout(
               sidebarPanel(
                 helpText("Combines all Monitary columns."),
                 downloadButton("download_combined", "Download Combined Totals (CSV)")
               ),
               mainPanel(DTOutput("table_combined"))
             )
    ),
    tabPanel("Standardized z-score Table",
             sidebarLayout(
               sidebarPanel(
                 helpText("Z-score standardization of Combined Total (mean = 0, sd = 1)."),
                 downloadButton("download_standardized", "Download Standardized Table (CSV)")
               ),
               mainPanel(DTOutput("table_standardized"))
             )
    ),
    tabPanel("Heat Map",
             sidebarLayout(
               sidebarPanel(
                 helpText("Heat map colored by Z_Score ")
               ),
               mainPanel(leafletOutput("map_zscore", height = 680))
             )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # --- Map (unique colors) ---
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
  
  # --- Load selected dataset ---
  neg_df <- reactive({
    readRDS(rds_paths_neg[input$year_neg])
  })
  
  # --- Numeric columns (excluding Population, CountyCode, Year, MuniCode) ---
  numeric_cols <- reactive({
    df <- as.data.frame(neg_df())
    exclude_cols <- c("MuniCode", "Population", "CountyCode", "Year", "year", "countycode")
    df <- df[, setdiff(colnames(df), exclude_cols), drop = FALSE]
    cols <- names(df)[sapply(df, is.numeric)]
    cols
  })
  
  # --- Build sliders for numeric columns ---
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
  
  # --- Apply % adjustments ---
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
  
  # --- Aggregate to County level ---
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
  
  # --- Combined Totals ---
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
  
  # --- Standardized Table ---
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
  
  # --- Heat Map (Z-Score) ---
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
            "<b>%s</b><br/>Population: %s<br/>Z-Score: %0.2f σ",
            joined$county_name,
            ifelse(is.na(joined$Population), "—", formatC(joined$Population, big.mark = ",")),
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
        title = "Z-Score (σ)",
        opacity = 1
      )
  })
}

shinyApp(ui, server)