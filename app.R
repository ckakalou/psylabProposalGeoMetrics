# app.R
# ------------------------------------------------------------
# Executive Consortium Innovation Dashboard (EIS 2024)
# - 3 main tabs: Consortium builder / Maps & KPIs / Budget
# - EU + Associated country selection (2 dropdowns)
# - Implementation flag + separate implementation-only map/KPIs
# - Scenario save/load/delete at bottom of Builder tab INCLUDING budgets + partner type
# - Budget entry uses explicit editor (row select + numeric input + Save)
# ------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(plotly)

suppressWarnings(suppressMessages({
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Please run: install.packages('countrycode')")
  }
}))

# ---- Associated countries list (per user) ----
associated_countries <- c(
  "Albania","Armenia","Bosnia and Herzegovina","Canada","Faroe Islands","Georgia",
  "Iceland","Israel","Kosovo","Moldova","Montenegro","New Zealand","North Macedonia",
  "Norway","Republic of Korea","Serbia","Switzerland","Tunisia","Türkiye","Ukraine","United Kingdom"
)

# Names typically used by Natural Earth / Plotly polygons
assoc_name_map <- c(
  "Republic of Korea" = "South Korea",
  "Türkiye" = "Turkey"
)

associated_countries_display <- ifelse(
  associated_countries %in% names(assoc_name_map),
  unname(assoc_name_map[associated_countries]),
  associated_countries
) |>
  unique() |>
  sort()

# ---- Partner type categories ----
partner_type_choices <- c(
  "Academic / Research",
  "Clinical / Hospital",
  "SME",
  "Large Industry",
  "NGO / Patient Association",
  "Public Authority / Policy Maker",
  "Infrastructure / Data Platform",
  "Network / Cluster",
  "Other"
)

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
    ),
    iso3 = toupper(trimws(iso3)),
    iso3 = ifelse((is.na(iso3) | iso3 == "") & country == "Greece", "GRC", iso3),
    iso3 = ifelse((is.na(iso3) | iso3 == "") & country == "Cyprus", "CYP", iso3)
  )

country_choices <- sort(unique(eis$country))

# EIS tier colors
level_colors <- c(
  "Innovation Leader"  = "#5E3C99",
  "Strong Innovator"   = "#1F78B4",
  "Moderate Innovator" = "#FDBF00",
  "Emerging Innovator" = "#33A02C"
)

neutral_associated <- "#8DA0CB"   # active-neutral for associated partners
inactive_grey <- "#EAEAEA"        # reserved for non-participating

country_to_iso3 <- function(country_vec) {
  iso <- countrycode::countrycode(country_vec, origin = "country.name", destination = "iso3c", warn = FALSE)
  # Custom fixes
  iso[country_vec == "Kosovo"] <- "XKX"
  iso[country_vec == "Faroe Islands"] <- "FRO"
  iso[country_vec == "Turkey"] <- "TUR"
  iso[country_vec == "South Korea"] <- "KOR"
  iso
}

# Baseline map frame: EIS (EU) + Associated countries
baseline_map <- bind_rows(
  eis |>
    distinct(country, iso3, innovation_level) |>
    mutate(is_associated = FALSE),
  tibble::tibble(
    country = associated_countries_display,
    iso3 = country_to_iso3(associated_countries_display),
    innovation_level = "Associated Country",
    is_associated = TRUE
  )
) |>
  filter(!is.na(iso3), iso3 != "") |>
  distinct(iso3, .keep_all = TRUE)

# ---- Helpers ----
fmt_eur_scalar <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("0")
  formatC(x, format = "f", digits = 0, big.mark = ",")
}

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

# JS renderer for Budget column to avoid scientific notation and show commas
budget_render_js <- JS("
function(data,type,row,meta){
  if(data === null || data === undefined || data === '') return '';
  var x = parseFloat(data);
  if(isNaN(x)) return '';
  return x.toLocaleString('en-US', {maximumFractionDigits:0});
}
")

# ---- UI ----
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  div(
    style = "margin-bottom: 14px;",
    tags$h2(style = "margin:0;", "Consortium Innovation Dashboard"),
    tags$div(style = "opacity:0.75; margin-top:2px;", "EIS 2024 tiers + Associated countries • Europe view")
  ),
  
  tags$style(HTML("
  td.dt-check, th.dt-check {
    text-align: center !important;
    vertical-align: middle !important;
  }
")),
  
  navset_tab(
    id = "main_tabs",
    
    # ---------------- TAB 1: Builder ----------------
    nav_panel(
      "Consortium builder",
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Build consortium"),
          textInput("partner_name", "Partner name", placeholder = "e.g., University of X"),
          
          checkboxInput("is_associated", "Associated country", value = FALSE),
          
          conditionalPanel(
            condition = "input.is_associated == false",
            selectInput("country_eu", "Country", choices = country_choices)
          ),
          conditionalPanel(
            condition = "input.is_associated == true",
            selectInput("country_assoc", "Associated country", choices = associated_countries_display)
          ),
          
          checkboxInput("is_implementation", "Implementation / deployment partner", value = FALSE),
          
          selectInput(
            "partner_type",
            "Partner type",
            choices = partner_type_choices,
            selected = "Academic / Research"
          ),
          
          layout_columns(
            actionButton("add_partner", "Add partner", class = "btn-primary"),
            actionButton("delete_selected", "Delete selected", class = "btn-outline-danger"),
            actionButton("clear_all", "Clear all", class = "btn-danger")
          ),
          
          hr(),
          tags$div(style="opacity:0.75;",
                   "Budget is entered in the Budget tab (select a partner row and click Save budget).")
        ),
        
        card(
          card_header("Current consortium"),
          DTOutput("partners_table"),
          
          hr(),
          card_header("Saved scenarios"),
          selectInput("scenario_select", "Choose scenario", choices = character(0)),
          textInput("scenario_name", "New scenario name (optional)", placeholder = "e.g., EMPATH-ePRO (v1)"),
          layout_columns(
            actionButton("scenario_save", "Save", class = "btn-success"),
            actionButton("scenario_load", "Load", class = "btn-primary"),
            actionButton("scenario_delete", "Delete", class = "btn-outline-danger")
          )
        )
      )
    ),
    
    # ---------------- TAB 2: Maps & KPIs ----------------
    nav_panel(
      "Maps & KPIs",
      card(
        card_header("Consortium views"),
        navset_tab(
          id = "map_tabs",
          nav_panel(
            "Whole consortium",
            plotlyOutput("map_all", height = 560),
            div(style = "height:12px;"),
            uiOutput("kpis_all")
          ),
          nav_panel(
            "Implementation only",
            plotlyOutput("map_impl", height = 560),
            div(style = "height:12px;"),
            uiOutput("kpis_impl")
          )
        )
      )
    ),
    
    # ---------------- TAB 3: Budget ----------------
    nav_panel(
      "Budget",
      tags$style(HTML("
  td.dt-budget-empty {
    outline: 2px solid #FFF3B0;
    outline-offset: -2px;
    background: #FFFBEB;
  }

  /* Total budget attention state */
  .needs-total-budget .form-control {
    border: 2px solid #FFF3B0 !important;
    background: #FFFBEB !important;
    box-shadow: none !important;
  }
"))
      ,
      layout_columns(
        col_widths = c(5, 7),
        
        card(
          card_header("Budget inputs"),
          uiOutput("total_budget_ui"),
          div(style="height:10px;"),
          uiOutput("budget_kpis"),
          
          hr(),
          card_header("Edit partner budget"),
          tags$div(style="opacity:0.75; margin-bottom:8px;",
                   "Select a partner row in the table (right), enter a budget, then click Save."),
          uiOutput("budget_selected_partner"),
          numericInput("budget_value", "Budget (€) for selected partner", value = NA, min = 0, step = 1000),
          layout_columns(
            actionButton("budget_save_btn", "Save budget", class = "btn-primary"),
            actionButton("budget_clear_btn", "Clear budget", class = "btn-outline-danger")
          )
        ),
        
        card(
          card_header(tagList("Budget table", tags$span(style="opacity:0.7; font-weight:400; margin-left:8px; font-size:0.9rem;",
                                                        "Select a row to edit its budget"))),
          DTOutput("budget_table")
        )
      ),
      
      div(style="height:12px;"),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Budget by country"),
          plotlyOutput("budget_by_country", height = 360)
        ),
        card(
          card_header("Budget by partner type"),
          plotlyOutput("budget_by_type", height = 360)
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  partners_rv <- reactiveVal(
    tibble::tibble(
      partner = character(),
      country = character(),
      partner_type = character(),
      is_associated = logical(),
      is_implementation = logical(),
      budget_eur = numeric()
    )
  )
  
  # ---- Schema normaliser (backwards compatible with older saved scenarios) ----
  normalize_partners_df <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(tibble::tibble(
        partner = character(),
        country = character(),
        partner_type = character(),
        is_associated = logical(),
        is_implementation = logical(),
        budget_eur = numeric()
      ))
    }
    
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    
    if (!"partner" %in% names(df)) df$partner <- character(nrow(df))
    if (!"country" %in% names(df)) df$country <- character(nrow(df))
    if (!"partner_type" %in% names(df)) df$partner_type <- "Other"
    if (!"is_associated" %in% names(df)) df$is_associated <- FALSE
    if (!"is_implementation" %in% names(df)) df$is_implementation <- FALSE
    if (!"budget_eur" %in% names(df)) df$budget_eur <- NA_real_
    
    df$is_associated <- as.logical(df$is_associated)
    df$is_implementation <- as.logical(df$is_implementation)
    df$budget_eur <- suppressWarnings(as.numeric(df$budget_eur))
    
    tibble::as_tibble(df[, c("partner","country","partner_type","is_associated","is_implementation","budget_eur")])
  }
  
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
  
  observeEvent(input$scenario_save, {
    name <- trimws(input$scenario_name)
    if (identical(name, "") || is.na(name)) name <- input$scenario_select
    req(!is.null(name), nchar(name) > 0)
    
    saveRDS(normalize_partners_df(partners_rv()), scenario_path(name))
    refresh_scenario_dropdown(selected = name)
    updateTextInput(session, "scenario_name", value = "")
  })
  
  observeEvent(input$scenario_load, {
    req(input$scenario_select)
    p <- scenario_path(input$scenario_select)
    req(file.exists(p))
    
    partners_rv(normalize_partners_df(readRDS(p)))
    updateTextInput(session, "scenario_name", value = "")
    updateNumericInput(session, "budget_value", value = NA)
  })
  
  observeEvent(input$scenario_delete, {
    req(input$scenario_select)
    p <- scenario_path(input$scenario_select)
    if (file.exists(p)) file.remove(p)
    refresh_scenario_dropdown()
  })
  
  # ---- Add/delete/clear partners ----
  observeEvent(input$add_partner, {
    req(input$partner_name)
    chosen_country <- if (isTRUE(input$is_associated)) input$country_assoc else input$country_eu
    req(chosen_country)
    
    df <- normalize_partners_df(partners_rv()) |>
      bind_rows(tibble::tibble(
        partner = trimws(input$partner_name),
        country = chosen_country,
        partner_type = input$partner_type,
        is_associated = isTRUE(input$is_associated),
        is_implementation = isTRUE(input$is_implementation),
        budget_eur = NA_real_
      ))
    
    partners_rv(df)
    updateTextInput(session, "partner_name", value = "")
    updateCheckboxInput(session, "is_implementation", value = FALSE)
    updateSelectInput(session, "partner_type", selected = "Academic / Research")
  })
  
  observeEvent(input$clear_all, {
    partners_rv(tibble::tibble(
      partner = character(),
      country = character(),
      partner_type = character(),
      is_associated = logical(),
      is_implementation = logical(),
      budget_eur = numeric()
    ))
    updateNumericInput(session, "budget_value", value = NA)
  })
  
  observeEvent(input$delete_selected, {
    df <- normalize_partners_df(partners_rv())
    sel <- input$partners_table_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      partners_rv(df[-sel, , drop = FALSE])
      updateNumericInput(session, "budget_value", value = NA)
    }
  })
  
  # ---- Enrich partners with EIS tier + ISO3 ----
  partners_enriched <- reactive({
    df <- normalize_partners_df(partners_rv())
    
    df2 <- df |>
      left_join(eis |> select(country, innovation_level, iso3), by = "country")
    
    df2$innovation_level <- ifelse(
      is.na(df2$innovation_level) & (df2$is_associated %in% TRUE),
      "Associated Country",
      df2$innovation_level
    )
    
    missing_iso <- is.na(df2$iso3) | df2$iso3 == ""
    df2$iso3[missing_iso] <- country_to_iso3(df2$country[missing_iso])
    
    df2
  })
  
  # ---- Builder consortium table ----
  output$partners_table <- renderDT({
    df <- partners_enriched() |>
      transmute(
        Partner = partner,
        Country = country,
        `Partner type` = partner_type,
        `Budget (€)` = budget_eur,
        Implementation = ifelse(is_implementation %in% TRUE, "&#10003;", ""),
        Associated = ifelse(is_associated %in% TRUE, "&#10003;", "")
      )
    
    datatable(
      df,
      rownames = FALSE,
      selection = "multiple",
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = "tip",
        columnDefs = list(
          list(targets = 3, render = budget_render_js),
          list(targets = c(4, 5), className = "dt-check")
        )
      )
      
    )
  })
  
  # ---- KPIs: whole consortium ----
  output$kpis_all <- renderUI({
    df <- partners_enriched()
    
    total_partners <- nrow(df)
    countries_n <- dplyr::n_distinct(df$country)
    
    leaders  <- sum(df$innovation_level == "Innovation Leader", na.rm = TRUE)
    strong   <- sum(df$innovation_level == "Strong Innovator", na.rm = TRUE)
    moderate <- sum(df$innovation_level == "Moderate Innovator", na.rm = TRUE)
    emerging <- sum(df$innovation_level == "Emerging Innovator", na.rm = TRUE)
    associated_n <- sum(df$is_associated %in% TRUE, na.rm = TRUE)
    
    tagList(
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi_card("Total partners", total_partners, "#111827"),
        kpi_card("Countries represented", countries_n, "#111827"),
        kpi_card("Associated partners", associated_n, neutral_associated)
      ),
      div(style="height:12px;"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_card("Innovation Leaders", leaders, level_colors[["Innovation Leader"]]),
        kpi_card("Strong Innovators", strong, level_colors[["Strong Innovator"]]),
        kpi_card("Moderate Innovators", moderate, level_colors[["Moderate Innovator"]], fg = "#111827"),
        kpi_card("Emerging Innovators", emerging, level_colors[["Emerging Innovator"]])
      )
    )
  })
  
  # ---- KPIs: implementation only ----
  output$kpis_impl <- renderUI({
    df <- partners_enriched() |>
      filter(is_implementation %in% TRUE)
    
    total_partners <- nrow(df)
    countries_n <- dplyr::n_distinct(df$country)
    
    leaders  <- sum(df$innovation_level == "Innovation Leader", na.rm = TRUE)
    strong   <- sum(df$innovation_level == "Strong Innovator", na.rm = TRUE)
    moderate <- sum(df$innovation_level == "Moderate Innovator", na.rm = TRUE)
    emerging <- sum(df$innovation_level == "Emerging Innovator", na.rm = TRUE)
    associated_n <- sum(df$is_associated %in% TRUE, na.rm = TRUE)
    
    tagList(
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi_card("Implementation partners", total_partners, "#111827"),
        kpi_card("Countries", countries_n, "#111827"),
        kpi_card("Associated", associated_n, neutral_associated)
      ),
      div(style="height:12px;"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_card("Leaders", leaders, level_colors[["Innovation Leader"]]),
        kpi_card("Strong", strong, level_colors[["Strong Innovator"]]),
        kpi_card("Moderate", moderate, level_colors[["Moderate Innovator"]], fg = "#111827"),
        kpi_card("Emerging", emerging, level_colors[["Emerging Innovator"]])
      )
    )
  })
  
  # ---- Map data + map plot ----
  build_map_df <- function(partners_df) {
    counts <- partners_df |>
      filter(!is.na(iso3), iso3 != "") |>
      count(iso3, name = "partners")
    
    baseline_map |>
      left_join(counts, by = "iso3") |>
      mutate(partners = ifelse(is.na(partners), 0L, as.integer(partners)))
  }
  
  plot_map <- function(df) {
    hover <- paste0(
      "<b>", df$country, "</b><br>",
      "Category: ", df$innovation_level, "<br>",
      "Partners: ", df$partners,
      ifelse(df$partners == 0, "<br><i>No partners added</i>", "")
    )
    df$iso3 <- toupper(trimws(as.character(df$iso3)))
  
    level_order <- c("Emerging Innovator", "Moderate Innovator", "Strong Innovator", "Innovation Leader")
    df$level_factor <- factor(df$innovation_level, levels = level_order)
    df$level_code <- as.integer(df$level_factor)
    
    df$z_plot <- 0L
    df$z_plot[df$partners > 0 & df$innovation_level %in% level_order] <- df$level_code[df$partners > 0 & df$innovation_level %in% level_order]
    df$z_plot[df$partners > 0 & df$innovation_level == "Associated Country"] <- 5L
    
    cols <- unname(level_colors[level_order])
    colorscale <- list(
      list(0.00, inactive_grey), list(0.16, inactive_grey),
      list(0.17, cols[1]),       list(0.32, cols[1]),
      list(0.33, cols[2]),       list(0.48, cols[2]),
      list(0.49, cols[3]),       list(0.64, cols[3]),
      list(0.65, cols[4]),       list(0.80, cols[4]),
      list(0.81, neutral_associated), list(1.00, neutral_associated)
    )
    
    plot_ly(
      type = "choropleth",
      locationmode = "ISO-3",
      locations = df$iso3,
      z = df$z_plot,
      zmin = 0,
      zmax = 5,
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
  }
  
  output$map_all <- renderPlotly({
   # print(partners_enriched() |> count(country, iso3) |> arrange(desc(n)))
    
    plot_map(build_map_df(partners_enriched()))
  })
  
  output$map_impl <- renderPlotly({
    plot_map(build_map_df(partners_enriched() |> filter(is_implementation %in% TRUE)))
  })
  
  # ---- Budget tab: table selection + Save budget ----
  output$budget_table <- renderDT({
    df <- partners_enriched() |>
      transmute(
        Partner = partner,
        Country = country,
        `Partner type` = partner_type,
        `Budget (€)` = budget_eur,  # numeric
        Implementation = ifelse(is_implementation %in% TRUE, "&#10003;", ""),
        Associated = ifelse(is_associated %in% TRUE, "&#10003;", "")
      )
    
    datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      selection = "single",
      options = list(
        pageLength = 15,
        dom = "tip",
        autoWidth = TRUE,
        ordering = FALSE,
        columnDefs = list(
          list(targets = 3, render = budget_render_js),
          list(targets = c(4, 5), className = "dt-check")
        ),
        initComplete = JS("
          function(settings, json) {
            var api = this.api();
            function paint() {
              api.cells(null, 3).every(function() {
                var td = this.node();
                var v = this.data();
                $(td).removeClass('dt-budget-empty');
                if(v === null || v === undefined || v === '' ) {
                  $(td).addClass('dt-budget-empty');
                }
              });
            }
            paint();
            api.on('draw', paint);
          }
        ")
      )
    )
  })
  
  output$budget_selected_partner <- renderUI({
    df <- partners_enriched()
    sel <- input$budget_table_rows_selected
    if (is.null(sel) || length(sel) == 0 || nrow(df) == 0) {
      return(tags$div(style="color: rgba(17,24,39,0.6);", "No partner selected."))
    }
    tags$div(tags$b("Selected: "), df$partner[sel], " (", df$country[sel], ")")
  })
  
  observeEvent(input$budget_table_rows_selected, {
    df <- partners_enriched()
    sel <- input$budget_table_rows_selected
    if (!is.null(sel) && length(sel) == 1 && nrow(df) >= sel) {
      updateNumericInput(session, "budget_value", value = df$budget_eur[sel])
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$budget_save_btn, {
    sel <- input$budget_table_rows_selected
    req(sel, length(sel) == 1)
    
    df <- normalize_partners_df(partners_rv())
    df$budget_eur[sel] <- suppressWarnings(as.numeric(input$budget_value))
    partners_rv(df)
  })
  
  observeEvent(input$budget_clear_btn, {
    sel <- input$budget_table_rows_selected
    req(sel, length(sel) == 1)
    
    df <- normalize_partners_df(partners_rv())
    df$budget_eur[sel] <- NA_real_
    partners_rv(df)
    updateNumericInput(session, "budget_value", value = NA)
  })
  
  output$total_budget_ui <- renderUI({
  total <- suppressWarnings(as.numeric(input$total_budget))

  needs <- is.null(total) || is.na(total) || total <= 0

  div(
    class = if (isTRUE(needs)) "needs-total-budget" else "",
    numericInput(
      "total_budget",
      "Total budget (€)",
      value = if (isTRUE(needs)) 0 else total,
      min = 0,
      step = 1000
    )
  )
})

  
  output$budget_kpis <- renderUI({
    df <- partners_enriched()
    total <- suppressWarnings(as.numeric(input$total_budget))
    if (is.na(total)) total <- 0
    
    allocated <- sum(df$budget_eur, na.rm = TRUE)
    remaining <- total - allocated
    
    tagList(
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi_card("Allocated (€)", paste0("€", fmt_eur_scalar(allocated)), level_colors[["Moderate Innovator"]], fg = "#111827"),
        kpi_card("Remaining (€)", paste0("€", fmt_eur_scalar(remaining)), ifelse(remaining >= 0, "#16A34A", "#DC2626")),
        kpi_card("Total (€)", paste0("€", fmt_eur_scalar(total)), "#111827")
      )
    )
  })
  
  output$budget_by_country <- renderPlotly({
    df <- partners_enriched() |>
      mutate(budget_eur = suppressWarnings(as.numeric(budget_eur))) |>
      filter(!is.na(budget_eur), budget_eur > 0) |>
      group_by(country) |>
      summarise(budget_eur = sum(budget_eur, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(budget_eur))
    
    if (nrow(df) == 0) {
      return(
        plot_ly() |>
          layout(
            annotations = list(list(text = "Enter budgets to see this chart", x = 0.5, y = 0.5, showarrow = FALSE)),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }


    
    plot_ly(
      df,
      x = ~reorder(country, budget_eur),
      y = ~budget_eur,
      type = "bar",
      hovertemplate = "%{x}<br>€%{y:,.0f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Budget (€)"),
        margin = list(l = 50, r = 10, t = 10, b = 90)
      )
  })
  
  output$budget_by_type <- renderPlotly({
    df <- partners_enriched() |>
      mutate(
        budget_eur = suppressWarnings(as.numeric(budget_eur)),
        impl = ifelse(is_implementation %in% TRUE, "Implementation", "Other")
      ) |>
      filter(!is.na(budget_eur), budget_eur > 0) |>
      group_by(partner_type, impl) |>
      summarise(budget_eur = sum(budget_eur, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df) == 0) {
      return(
        plot_ly() |>
          layout(
            annotations = list(list(text = "Enter budgets to see this chart", x = 0.5, y = 0.5, showarrow = FALSE)),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    plot_ly(
      df,
      x = ~partner_type,
      y = ~budget_eur,
      color = ~impl,
      type = "bar",
      barmode = "stack",
      hovertemplate = "%{x}<br>%{color}: €%{y:,.0f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", tickangle = -25),
        yaxis = list(title = "Budget (€)"),
        margin = list(l = 50, r = 10, t = 10, b = 90)
      )
  })
}

shinyApp(ui, server)
