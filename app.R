# app.R
# ------------------------------------------------------------
# Executive Consortium Innovation Dashboard (EIS 2024)
# - Flat choropleth (Plotly) with Europe viewport (no overseas territories visible)
# - Consortium builder + table (left)
# - Map + executive KPIs (right; KPIs under map)
# - Scenario save/load/delete (file-based, local + shinyapps.io)
# ------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(plotly)

# ---- Load EIS mapping (country, iso3, innovation_level) ----
eis <- read.csv("EIS2024_country_iso3_innovation_level.csv", stringsAsFactors = FALSE) |>
  as_tibble() |>
  mutate(
    country = trimws(country),
    innovation_level = dplyr::recode(
      innovation_level,
      "Leader"   = "Innovation Leader",
      "Strong"   = "Strong Innovator",
      "Moderate" = "Moderate Innovator",
      "Emerging" = "Emerging Innovator",
      .default = innovation_level
    )
  ) |>
  mutate(
    iso3 = toupper(trimws(iso3)),
    iso3 = ifelse((is.na(iso3) | iso3 == "") & country == "Greece", "GRC", iso3),
    iso3 = ifelse((is.na(iso3) | iso3 == "") & country == "Cyprus", "CYP", iso3)
  )

country_choices <- sort(unique(eis$country))

level_colors <- c(
  "Innovation Leader"  = "#5E3C99",
  "Strong Innovator"   = "#1F78B4",
  "Moderate Innovator" = "#FDBF00",
  "Emerging Innovator" = "#33A02C"
)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  div(
    style = "margin-bottom: 14px;",
    tags$h2(style = "margin:0;", "Consortium Innovation Dashboard"),
    tags$div(style = "opacity:0.75; margin-top:2px;", "EIS 2024 innovation levels • Europe view")
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    
    # ---------------- LEFT PANEL ----------------
    card(
      card_header("Consortium Builder"),
      textInput("partner_name", "Partner name", placeholder = "e.g., University of X"),
      selectInput("country", "Country", choices = country_choices),
      
      layout_columns(
        actionButton("add_partner", "Add partner", class = "btn-primary"),
        actionButton("delete_selected", "Delete selected", class = "btn-outline-danger"),
        actionButton("clear_all", "Clear all", class = "btn-danger")
      ),
      
      hr(),
      card_header("Saved scenarios"),
      selectInput("scenario_select", "Choose scenario", choices = character(0)),
      textInput("scenario_name", "New scenario name", placeholder = "e.g., EMPATH-ePRO (v1)"),
      layout_columns(
        actionButton("scenario_save", "Save", class = "btn-success"),
        actionButton("scenario_load", "Load", class = "btn-primary"),
        actionButton("scenario_delete", "Delete", class = "btn-outline-danger")
      ),
      
      hr(),
      card_header("Current consortium"),
      DTOutput("partners_table")
    ),
    
    # ---------------- RIGHT PANEL ----------------
    tagList(
      card(
        card_header(
          tagList(
            tags$span("Europe Innovation Map"),
            tags$span(style = "opacity:0.7; font-weight:400; margin-left:8px; font-size:0.9rem;", "EIS 2024 levels")
          )
        ),
        plotlyOutput("map", height = 600)
      ),
      
      div(style = "height:12px;"),
      
      card(
        card_header("Summary KPIs"),
        uiOutput("kpis")
      )
    )
  )
)

server <- function(input, output, session) {
  
  partners_rv <- reactiveVal(
    tibble::tibble(partner = character(), country = character())
  )
  
  # ---- Scenario save/load (file-based) ----
  scenario_dir <- "scenarios"
  dir.create(scenario_dir, showWarnings = FALSE)
  
  scenario_path <- function(name) {
    safe <- gsub("[^A-Za-z0-9 _-]", "", name)
    safe <- trimws(safe)
    file.path(scenario_dir, paste0(safe, ".rds"))
  }
  
  list_scenarios <- function() {
    files <- list.files(scenario_dir, pattern = "\\.rds$", full.names = FALSE)
    sort(sub("\\.rds$", "", files))
  }
  
  refresh_scenario_dropdown <- function(selected = NULL) {
    choices <- list_scenarios()
    sel <- if (!is.null(selected) && selected %in% choices) selected else if (length(choices) > 0) choices[[1]] else character(0)
    updateSelectInput(session, "scenario_select", choices = choices, selected = sel)
  }
  
  refresh_scenario_dropdown()
  current_scenario <- reactiveVal(NULL)
  
  observeEvent(input$scenario_save, {
    name <- trimws(input$scenario_name)
    
    # If user didn't type a new name, overwrite the selected/current scenario
    if (identical(name, "") || is.na(name)) {
      name <- input$scenario_select
    }
    
    req(!is.null(name), nchar(name) > 0)
    
    saveRDS(partners_rv(), scenario_path(name))
    
    refresh_scenario_dropdown(selected = name)
    current_scenario(name)
    updateTextInput(session, "scenario_name", value = "")
  })
  
  
  observeEvent(input$scenario_load, {
    req(input$scenario_select)
    p <- scenario_path(input$scenario_select)
    req(file.exists(p))
    
    partners_rv(readRDS(p))
    current_scenario(input$scenario_select)
    
    # Optional UX: clear the name box so Save overwrites the selected scenario
    updateTextInput(session, "scenario_name", value = "")
  })
  
  observeEvent(input$scenario_select, {
    if (!is.null(input$scenario_select) && nzchar(input$scenario_select)) {
      current_scenario(input$scenario_select)
    }
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$scenario_delete, {
    req(input$scenario_select)
    p <- scenario_path(input$scenario_select)
    if (file.exists(p)) file.remove(p)
    refresh_scenario_dropdown()
  })
  
  # ---- Consortium actions ----
  observeEvent(input$add_partner, {
    req(input$partner_name, input$country)
    df <- partners_rv() |>
      bind_rows(tibble::tibble(
        partner = trimws(input$partner_name),
        country = input$country
      ))
    partners_rv(df)
    updateTextInput(session, "partner_name", value = "")
  })
  
  observeEvent(input$clear_all, {
    partners_rv(tibble::tibble(partner = character(), country = character()))
  })
  
  observeEvent(input$delete_selected, {
    df <- partners_rv()
    sel <- input$partners_table_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      partners_rv(df[-sel, , drop = FALSE])
    }
  })
  
  partners_enriched <- reactive({
    partners_rv() |>
      left_join(eis, by = "country")
  })
  
  # ---- Summary KPIs ----
  output$kpis <- renderUI({
    df <- partners_enriched()
    
    total_partners <- nrow(df)
    countries_n <- dplyr::n_distinct(df$country)
    
    leaders  <- sum(df$innovation_level == "Innovation Leader", na.rm = TRUE)
    strong   <- sum(df$innovation_level == "Strong Innovator", na.rm = TRUE)
    moderate <- sum(df$innovation_level == "Moderate Innovator", na.rm = TRUE)
    emerging <- sum(df$innovation_level == "Emerging Innovator", na.rm = TRUE)
    
    kpi_card <- function(title, value, bg, fg = "white") {
      bslib::card(
        class = "h-100",
        style = paste0(
          "background:", bg, "; color:", fg, "; ",
          "border: 0; border-radius: 16px; ",
          "box-shadow: 0 6px 18px rgba(0,0,0,0.08);"
        ),
        bslib::card_body(
          tags$div(style = "font-size: 0.85rem; opacity: 0.9;", title),
          tags$div(style = "font-size: 2.0rem; font-weight: 800; line-height: 1.1; margin-top: 4px;", value)
        )
      )
    }
    
    tagList(
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi_card("Total partners", total_partners, "#50C5B7"),
        kpi_card("Countries represented", countries_n, "#111827"),
        kpi_card("Innovation Leaders", leaders, level_colors[["Innovation Leader"]])
      ),
      div(style = "height:12px;"),
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi_card("Strong Innovators", strong, level_colors[["Strong Innovator"]]),
        kpi_card("Moderate Innovators", moderate, level_colors[["Moderate Innovator"]], fg = "#111827"),
        kpi_card("Emerging Innovators", emerging, level_colors[["Emerging Innovator"]])
      )
    )
  })
  
  # ---- Consortium table (no iso3) ----
  output$partners_table <- renderDT({
    df <- partners_enriched() |>
      select(partner, country, innovation_level)
    
    datatable(
      df,
      rownames = FALSE,
      selection = "multiple",
      options = list(pageLength = 10, dom = "tip")
    )
  })
  
  # ---- Map data (EIS levels + partner counts) ----
  map_df <- reactive({
    counts <- partners_enriched() |>
      count(iso3, name = "partners")
    
    eis |>
      distinct(country, iso3, innovation_level) |>
      left_join(counts, by = "iso3") |>
      mutate(partners = ifelse(is.na(partners), 0, partners))
  })
  
  output$map <- renderPlotly({
    df <- map_df()
    
    # Make sure partners is numeric and non-NA
    df$partners <- as.integer(df$partners)
    df$partners[is.na(df$partners)] <- 0
    
    # Use one consistent order for factor coding AND colors
    level_order <- c(
      "Emerging Innovator",
      "Moderate Innovator",
      "Strong Innovator",
      "Innovation Leader"
    )
    
    df$level_factor <- factor(df$innovation_level, levels = level_order)
    df$level_code <- as.integer(df$level_factor)  # 1..4
    
    # Grey when no partners at all; otherwise color only countries with partners
    if (sum(df$partners) == 0) {
      df$z_plot <- 0
    } else {
      df$z_plot <- ifelse(df$partners > 0, df$level_code, 0)
    }
    df$z_plot[is.na(df$z_plot)] <- 0
    
    hover <- paste0(
      "<b>", df$country, "</b><br>",
      "Innovation level: ", df$innovation_level, "<br>",
      "Partners: ", df$partners,
      ifelse(df$partners == 0, "<br><i>No partners added</i>", "")
    )
    
    # Colors in EXACTLY the same order as level_order
    cols <- unname(level_colors[level_order])
    
    # 0 = grey, 1..4 = Emerging..Leader
    colorscale <- list(
      list(0.00, "#EAEAEA"), list(0.1999, "#EAEAEA"),
      list(0.20, cols[1]),   list(0.3999, cols[1]),
      list(0.40, cols[2]),   list(0.5999, cols[2]),
      list(0.60, cols[3]),   list(0.7999, cols[3]),
      list(0.80, cols[4]),   list(1.00, cols[4])
    )
    
    plot_ly(
      type = "choropleth",
      locations = df$iso3,
      z = df$z_plot,
      zmin = 0,   # IMPORTANT: fixes Plotly re-normalizing when a level is absent
      zmax = 4,
      text = hover,
      hoverinfo = "text",
      colorscale = colorscale,
      showscale = FALSE,
      marker = list(line = list(color = "white", width = 0.8))
    ) |>
      layout(
        geo = list(
          scope = "world",
          projection = list(type = "mercator"),
          showframe = FALSE,
          showcoastlines = FALSE,
          showcountries = TRUE,
          countrycolor = "white",
          lataxis = list(range = c(34, 72), fixedrange = TRUE),
          lonaxis = list(range = c(-25, 45), fixedrange = TRUE)
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  })
  
}

shinyApp(ui, server)
