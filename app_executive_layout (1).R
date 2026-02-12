# app.R
# ------------------------------------------------------------
# Consortium dashboard: EIS 2024 innovation level (EU) + Europe-only choropleth
# - Partner entry table (name + country)
# - Country-level choropleth (Plotly) styled like EIS (no basemap tiles)
# - Robust ISO3 mapping (iso3)
# - Cyprus selectable: dropdown uses trimmed unique country names
# - Exports: CSV + HTML report + PDF attempt (with HTML fallback message)
# ------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(rmarkdown)
library(plotly)

# ---- 1) Load EIS mapping (country, iso3, innovation_level) ----
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
    # Fix common EIS/FP naming quirks: Greece often appears as EL in EU contexts
    iso3 = ifelse((is.na(iso3) | iso3 == "") & country == "Greece", "GRC", iso3),
    iso3 = ifelse((is.na(iso3) | iso3 == "") & country == "Cyprus", "CYP", iso3)
  )

# Country dropdown options (unique + sorted) — ensures Cyprus is selectable if present in CSV
country_choices <- sort(unique(eis$country))

# ---- 2) Colors for innovation levels (EIS-style) ----
level_colors <- c(
  "Innovation Leader"  = "#5E3C99",  # purple
  "Strong Innovator"   = "#1F78B4",  # blue
  "Moderate Innovator" = "#FDBF00",  # yellow
  "Emerging Innovator" = "#33A02C"   # green
)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  div(style="margin-bottom: 14px;", tags$h2(style="margin:0;", "Consortium Innovation Dashboard"), tags$div(style="opacity:0.75; margin-top:2px;", "EIS 2024 innovation levels • Europe view")),

  layout_columns(
    col_widths = c(6, 6),

    # ---------------- Left column ----------------
    card(
      card_header("Consortium Builder"),
      textInput("partner_name", "Partner name", placeholder = "e.g., University of X"),
      selectInput("country", "Country", choices = country_choices),

      layout_columns(
        actionButton("add_partner", "Add partner", class = "btn-primary"),
        actionButton("delete_selected", "Delete selected", class = "btn-outline-danger"),
        actionButton("clear_all", "Clear all", class = "btn-danger")
      ),

      layout_columns(
        downloadButton("download_cons", "Export consortium (CSV)", class = "btn-success"),
        downloadButton("download_report_pdf", "Export report (PDF)", class = "btn-dark"),
        downloadButton("download_report_html", "Export report (HTML)", class = "btn-outline-secondary")
      ),
      hr(),

      card_header("Current consortium"),
      DTOutput("partners_table")
    ),

    # ---------------- Right column ----------------
    
# ---------------- RIGHT PANEL ----------------
tagList(
  card(
    card_header(tagList(tags$span("Europe Innovation Map"), tags$span(style="opacity:0.7; font-weight:400; margin-left:8px; font-size:0.9rem;", "EIS 2024 levels"))),
    plotlyOutput("map", height = 600)
  ),

  div(style="height:12px;"),

  card(
    card_header("Executive KPIs"),
    uiOutput("kpis")
  )
)
)

server <- function(input, output, session) {

  partners_rv <- reactiveVal(
    tibble::tibble(partner = character(), country = character())
  )

  observeEvent(input$add_partner, {
    req(input$partner_name, input$country)

    df <- partners_rv()
    df <- bind_rows(df, tibble::tibble(
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
      df <- df[-sel, , drop = FALSE]
      partners_rv(df)
    }
  })

  partners_enriched <- reactive({
    partners_rv() |>
      left_join(eis, by = "country")
  })

  # ---- KPIs ----
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

  # Executive: 2 rows x 3 cards
  tagList(
    layout_columns(
      col_widths = c(4,4,4),
      kpi_card("Total partners", total_partners, "#111827"),
      kpi_card("Countries represented", countries_n, "#111827"),
      kpi_card("Innovation Leaders", leaders, "#5E3C99")
    ),
    div(style="height:12px;"),
    layout_columns(
      col_widths = c(4,4,4),
      kpi_card("Strong Innovators", strong, "#1F78B4"),
      kpi_card("Moderate Innovators", moderate, "#FDBF00", fg = "#111827"),
      kpi_card("Emerging Innovators", emerging, "#33A02C")
    )
  )
})# ---- Consortium table ----
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

  # ---- Summaries ----
  output$level_summary <- renderDT({
    partners_enriched() |>
      count(innovation_level, name = "partners") |>
      arrange(desc(partners)) |>
      datatable(rownames = FALSE, options = list(dom = "t"))
  })

  output$country_summary <- renderDT({
    partners_enriched() |>
      count(country, iso3, innovation_level, name = "partners") |>
      arrange(desc(partners), country) |>
      datatable(rownames = FALSE, options = list(pageLength = 10))
  })

  # ---- Map data: count partners per iso3 ----
  map_df <- reactive({
    counts <- partners_enriched() |>
      count(country, iso3, innovation_level, name = "partners")

    # Include all EIS countries even if zero partners so the map shows full Europe
    eis |>
      distinct(country, iso3, innovation_level) |>
      left_join(counts |> select(iso3, partners), by = "iso3") |>
      mutate(partners = ifelse(is.na(partners), 0, partners))
  })

  # ---- Plotly choropleth styled like EIS (no basemap) ----
  output$map <- renderPlotly({
    df <- map_df()

    # Hover text
    hover <- paste0(
      "<b>", df$country, "</b><br>",
      "Innovation level: ", df$innovation_level, "<br>",
      "Partners: ", df$partners
    )

    level_order <- c("Emerging Innovator", "Moderate Innovator", "Strong Innovator", "Innovation Leader")
    df$level_factor <- factor(df$innovation_level, levels = level_order)
    df$level_code <- as.integer(df$level_factor)

    cols <- level_colors[level_order]
    colorscale <- list(
      list(0.00, cols[1]), list(0.24, cols[1]),
      list(0.25, cols[2]), list(0.49, cols[2]),
      list(0.50, cols[3]), list(0.74, cols[3]),
      list(0.75, cols[4]), list(1.00, cols[4])
    )

    plot_ly(
      type = "choropleth",
      locations = df$iso3,
      z = df$level_code,
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
          fitbounds = "locations",
          lataxis = list(range = c(30, 75)),
          lonaxis = list(range = c(-32, 52))
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  })

  # ---- CSV export ----
  output$download_cons <- downloadHandler(
    filename = function() {
      paste0("consortium_export_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      partners_enriched() |>
        select(partner, country, innovation_level) |>
        write.csv(file, row.names = FALSE)
    }
  )

  # ---- Report tables ----
  report_tables <- reactive({
    data_tbl <- partners_enriched() |>
      select(partner, country, innovation_level)

    level_tbl <- data_tbl |>
      count(innovation_level, name = "partners") |>
      arrange(desc(partners))

    country_tbl <- data_tbl |>
      count(country, iso3, innovation_level, name = "partners") |>
      arrange(desc(partners), country)

    list(data_tbl = data_tbl, level_tbl = level_tbl, country_tbl = country_tbl)
  })

  # ---- HTML report export (robust everywhere) ----
  output$download_report_html <- downloadHandler(
    filename = function() {
      paste0("consortium_report_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
    },
    content = function(file) {
      tmp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("consortium_report.Rmd", tmp_rmd, overwrite = TRUE)

      tabs <- report_tables()

      rmarkdown::render(
        input = tmp_rmd,
        output_format = "html_document",
        output_file = file,
        params = list(
          data = tabs$data_tbl,
          level_summary = tabs$level_tbl,
          country_summary = tabs$country_tbl
        ),
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
    }
  )

  # ---- PDF report export (tries pagedown; writes guidance text if unavailable) ----
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      paste0("consortium_report_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
    },
    content = function(file) {
      tmp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("consortium_report.Rmd", tmp_rmd, overwrite = TRUE)

      tabs <- report_tables()

      ok <- TRUE
      msg <- NULL

      tryCatch({
        if (!requireNamespace("pagedown", quietly = TRUE)) {
          stop("Package 'pagedown' is not installed on this system/server.")
        }

        rmarkdown::render(
          input = tmp_rmd,
          output_format = pagedown::chrome_print(),
          output_file = file,
          params = list(
            data = tabs$data_tbl,
            level_summary = tabs$level_tbl,
            country_summary = tabs$country_tbl
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
      }, error = function(e) {
        ok <<- FALSE
        msg <<- conditionMessage(e)
      })

      if (!ok) {
        writeLines(
          c(
            "PDF report could not be generated on this system/server.",
            "",
            "Reason:",
            msg,
            "",
            "Workaround:",
            "1) Download the HTML report from the app.",
            "2) Open it in your browser.",
            "3) Print -> Save as PDF."
          ),
          con = file
        )
      }
    }
  )
}

shinyApp(ui, server)
