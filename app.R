library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(readr)

source("R/score.R")
source("R/briefing.R")

schools <- read_csv(
  "data/processed/ca_schools.csv",
  col_types = cols(
    county_code = col_character(),
    district_code = col_character(),
    school_code = col_character(),
    zip = col_character(),
    cds = col_character()
  ),
  na = c("", "NA")
)

zip_centroids <- read_csv(
  "data/processed/ca_zips.csv",
  col_types = cols(zip = col_character()),
  na = c("", "NA")
)

us_states <- read_csv(
  "data/processed/us_states.csv",
  col_types = cols(state_abbr = col_character()),
  na = c("", "NA")
)

schools_prior <- read_csv(
  "data/processed/ca_schools_prior.csv",
  col_types = cols(cds = col_character()),
  na = c("", "NA")
)

classroom_template <- read_csv(
  "data/processed/classroom_template.csv",
  na = c("", "NA")
)

caaspp_state_pct <- schools$state_pct_ela[[1]]
caaspp_prior_pct <- if ("state_pct_ela" %in% names(schools_prior)) {
  schools_prior$state_pct_ela[[1]]
} else {
  47.04
}
naep_nation <- us_states$nation_pct_reading[[1]]
state_choices <- c(
  "All states" = "US",
  setNames(
    us_states$state_abbr,
    paste0(us_states$state_name, " (", us_states$state_abbr, ")")
  )
)
county_choices <- c("All counties" = "", sort(unique(schools$county_name)))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x[[1]]))) y else x
}

pct_label <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (length(x) == 0 || is.na(x)) "Suppressed / n < 11" else sprintf("%.1f%%", x)
}

pts_label <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (length(x) == 0 || is.na(x)) "Suppressed / n < 11" else sprintf("%+.1f pts", x)
}

need_label <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (length(x) == 0 || is.na(x)) "n/a" else sprintf("%.0f", x)
}

count_label <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (length(x) == 0 || is.na(x)) "—" else format(round(x), big.mark = ",")
}

named_choices <- function(values, labels) {
  values <- as.character(values)
  labels <- as.character(labels)
  n <- min(length(values), length(labels))
  if (n == 0) {
    return(character())
  }
  setNames(values[seq_len(n)], labels[seq_len(n)])
}

need_col <- function(district_filter) {
  if (is.null(district_filter) || district_filter == "") "need_score_ca" else "need_score_district"
}

ui <- page_navbar(
  id = "main_nav",
  title = "Literacy Gap Analyzer",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1b4f72"
  ),
  sidebar = sidebar(
    title = "Place",
    selectInput("state", "State", choices = state_choices, selected = "US"),
    selectInput("county", "County (California schools)", choices = county_choices, selected = ""),
    selectInput("district", "District", choices = c("All districts" = "")),
    selectInput("zip", "ZIP code", choices = c("All ZIPs" = "")),
    selectInput("school", "School", choices = c("All schools" = "")),
    sliderInput(
      "need_min",
      "Minimum need score",
      min = 0,
      max = 100,
      value = 0,
      step = 1
    ),
    checkboxInput("hide_alt", "Hide continuation / alternative schools", value = FALSE),
    actionButton("focus_srvusd", "Focus San Ramon Valley Unified", class = "btn-primary btn-sm"),
    hr(),
    p(class = "small text-muted", "All states use NAEP 2024 reading (comparable). California also has school-level CAASPP ELA. Pick a district for within-district school need.")
  ),
  # Leaflet inside a hidden nav tab often paints at 0×0; refresh size when Map opens.
  header = tagList(
    tags$script(HTML(
      "
      function lgaInvalidateMap() {
        setTimeout(function() {
          var el = document.getElementById('zip_map');
          if (!el || !window.HTMLWidgets) return;
          var widget = HTMLWidgets.find('#zip_map');
          if (widget && widget.getMap) {
            widget.getMap().invalidateSize();
          }
        }, 250);
      }
      $(document).on('shiny:value', function(e) {
        if (e.name === 'zip_map') lgaInvalidateMap();
      });
      $(document).on('shown.bs.tab', 'button[data-bs-toggle=\"tab\"]', function(e) {
        var label = ($(e.target).text() || '').trim();
        if (label === 'Map') lgaInvalidateMap();
      });
      "
    )),
    tags$div(
      class = "px-3 pt-3",
      tags$p(
        class = "text-muted mb-0",
        "Congressional App Challenge · CA-10 · NAEP reading for every state · CAASPP ELA for California schools"
      )
    )
  ),
  nav_panel(
    "Overview",
    fillable = FALSE,
    uiOutput("overview")
  ),
  nav_panel(
    "Map",
    fillable = FALSE,
    card(
      full_screen = TRUE,
      card_header("Map: all states = NAEP need by state; California = school ZIPs"),
      leafletOutput("zip_map", height = "70vh"),
      p(class = "small text-muted mb-0", "Click a ZIP to filter. Centroids are Census ZCTAs, so they are near—not exact—school sites.")
    )
  ),
  nav_panel(
    "Rankings",
    fillable = FALSE,
    card(
      card_header(textOutput("table_header")),
      div(style = "min-height: 60vh;", DTOutput("table"))
    )
  ),
  nav_panel(
    "Profile",
    fillable = FALSE,
    # Keep plotOutput outside renderUI; fixed plot size avoids invalid height = 0
    # when the tab first becomes visible inside a bslib layout.
    div(
      style = "min-height: 70vh; overflow: auto; padding: 0.25rem 0.5rem;",
      uiOutput("profile"),
      plotOutput("profile_subgroups", height = "300px", width = "100%")
    )
  ),
  nav_panel(
    "Factors",
    fillable = FALSE,
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("ELA proficiency vs economic gap"),
        plotOutput("plot_econ", height = 360),
        p(class = "small text-muted", "Each point is a school in the current place filter.")
      ),
      card(
        card_header("ELA proficiency vs English learner gap"),
        plotOutput("plot_ell", height = 360),
        p(class = "small text-muted", "Points may be missing when CDE suppresses small groups.")
      )
    )
  ),
    nav_panel(
      "Compare",
      fillable = FALSE,
      card(
        card_header("Compare two schools, two states, or two years"),
        uiOutput("compare_controls")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Bars"), plotOutput("compare_plot", height = "420px")),
        card(card_header("Numbers"), DTOutput("compare_table"))
      )
    ),
    nav_panel(
      "Briefing",
      fillable = FALSE,
      card(
        card_header("One-page download for a staffer"),
        p("Uses the school or state currently selected in the sidebar (or the last map click). PDF is one printable page; CSV is the same facts in a spreadsheet."),
        downloadButton("briefing_pdf", "Download PDF"),
        downloadButton("briefing_csv", "Download CSV")
      ),
      card(card_header("Preview"), verbatimTextOutput("briefing_preview"))
    ),
    nav_panel(
      "Classroom",
      fillable = FALSE,
      card(
        card_header("Upload a demo classroom CSV"),
        p("Columns: school_name, n_tested, pct_ela_all, pct_ela_sed, pct_ela_not_sed, pct_ela_ell, pct_ela_fluent. Need scores are ranked only among the uploaded rows, then compared with California’s CAASPP ELA average."),
        downloadButton("classroom_template_dl", "Download template CSV"),
        fileInput("classroom_file", "Upload CSV", accept = ".csv")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Classroom vs California average"), plotOutput("classroom_plot", height = "360px")),
        card(card_header("Scored rooms"), DTOutput("classroom_table"))
      )
    ),
    nav_panel(
      "Methods",
    fillable = FALSE,
    card(
      h4("Geography"),
      tags$ul(
        tags$li("United States: NAEP 2024 reading, grades 4 and 8, percent at or above Proficient for public schools in all 50 states and D.C."),
        tags$li("California schools: 2024–25 CAASPP Smarter Balanced ELA (school, district, and charter types 7/9/10)"),
        tags$li("Contest home district: San Ramon Valley Unified (CDS 07-61804), Contra Costa County, California’s 10th Congressional District"),
        tags$li("California ZIP map uses Census 2024 ZCTA internal points; the All-states map uses approximate state centers")
      ),
      h4("Data"),
      tags$ul(
        tags$li(
          "NAEP Data Service API (2024 reading), ",
          a("nationsreportcard.gov", href = "https://www.nationsreportcard.gov/api_documentation.aspx", target = "_blank")
        ),
        tags$li(
          "Statewide CAASPP ELA research file, caret-delimited, ",
          a("CDE / ETS", href = "https://caaspp-elpac.ets.org/caaspp/ResearchFileListSB?lstTestYear=2025&ps=true", target = "_blank")
        ),
        tags$li("Test ID 1 = ELA; Grade 13 = all tested grades combined; Student Group 1 = all students"),
        tags$li("Student Group 31 = socioeconomically disadvantaged; 111 = not SED"),
        tags$li("Student Group 160 = English learner (excluding RFEP); 6 = IFEP, RFEP, and English-only"),
        tags$li("Year compare uses 2023–24 CAASPP ELA (same groups) and NAEP 2022 vs 2024 reading")
      ),
      HTML(need_formula_html()),
      h4("What we did not claim"),
      tags$ul(
        tags$li("This is not a forecast that scores will fall next year."),
        tags$li("Public NAEP cannot identify individual schools. Do not compare NAEP Proficient rates with CAASPP met-and-above rates as if they were the same test."),
        tags$li("ZIP markers are ZCTA centroids, not building footprints.")
      ),
      h4("Rebuild the tables"),
      tags$pre("python3 scripts/prepare_us_states.py\nbash scripts/download_caaspp.sh\npython3 scripts/prepare_data.py\npython3 scripts/prepare_trends.py")
    )
  )
)

server <- function(input, output, session) {
  selected_cds <- reactiveVal(NULL)
  selected_state_abbr <- reactiveVal(NULL)
  pending_district <- reactiveVal(NULL)
  pending_zip <- reactiveVal(NULL)

  is_ca <- reactive(identical(input$state, "CA"))
  is_all_states <- reactive(is.null(input$state) || identical(input$state, "US"))

  observeEvent(input$state, {
    if (!identical(input$state, "CA")) {
      updateSelectInput(session, "county", selected = "")
      updateSelectInput(session, "district", selected = "")
      updateSelectInput(session, "zip", selected = "")
      updateSelectInput(session, "school", selected = "")
      selected_cds(NULL)
    }
    if (!is.null(input$state) && !identical(input$state, "US") && input$state != "") {
      selected_state_abbr(input$state)
    }
  }, ignoreInit = TRUE)

  geo <- reactive({
    df <- schools
    if (isTRUE(input$hide_alt)) {
      df <- df |> filter(!grepl("Continuation|Alternative", school_name, ignore.case = TRUE))
    }
    if (!is.null(input$county) && input$county != "") {
      df <- df |> filter(county_name == input$county)
    }
    if (!is.null(input$district) && input$district != "") {
      df <- df |> filter(district_name == input$district)
    }
    df
  })

  observeEvent(geo(), {
    df <- geo()
    county_selected <- !is.null(input$county) && input$county != ""
    districts <- if (county_selected) sort(unique(df$district_name)) else character()
    pd <- pending_district()
    keep <- if (!is.null(pd) && pd %in% districts) {
      pending_district(NULL)
      pd
    } else if (!is.null(input$district) && input$district %in% districts) {
      input$district
    } else {
      ""
    }
    updateSelectInput(
      session,
      "district",
      choices = c("All districts" = "", districts),
      selected = keep
    )
    zips <- if (county_selected) {
      sort(unique(df$zip[!is.na(df$zip) & df$zip != ""]))
    } else {
      character()
    }
    pd_zip <- pending_zip()
    zip_keep <- if (!is.null(pd_zip) && pd_zip %in% zips) {
      pending_zip(NULL)
      pd_zip
    } else if (!is.null(input$zip) && input$zip %in% zips) {
      input$zip
    } else {
      ""
    }
    updateSelectInput(session, "zip", choices = c("All ZIPs" = "", zips), selected = zip_keep)
    school_df <- df
    if (zip_keep != "") {
      school_df <- school_df |> filter(zip == zip_keep)
    }
    school_choices <- named_choices(
      school_df$cds,
      paste0(school_df$school_name, " (", school_df$district_name, ")")
    )
    if (length(school_choices) > 0) {
      school_choices <- school_choices[order(names(school_choices))]
    }
    if (nrow(school_df) > 400) {
      updateSelectInput(session, "school", choices = c("All schools (pick from table)" = ""), selected = "")
    } else {
      keep_school <- if (!is.null(input$school) && input$school %in% school_df$cds) input$school else ""
      updateSelectInput(
        session,
        "school",
        choices = c("All schools" = "", school_choices),
        selected = keep_school
      )
    }
  }, ignoreInit = FALSE)

  observeEvent(input$focus_srvusd, {
    pending_district("San Ramon Valley Unified")
    selected_cds(NULL)
    selected_state_abbr("CA")
    updateSelectInput(session, "state", selected = "CA")
    updateSelectInput(session, "zip", selected = "")
    updateSelectInput(session, "school", selected = "")
    updateSelectInput(session, "county", selected = "Contra Costa")
    # District choices populate after county updates; set explicitly once flushed.
    session$onFlushed(function() {
      updateSelectInput(session, "district", selected = "San Ramon Valley Unified")
    }, once = TRUE)
  })

  filtered <- reactive({
    df <- geo()
    col <- need_col(input$district)
    if (!is.null(input$zip) && input$zip != "") {
      df <- df |> filter(zip == input$zip)
    }
    df <- df |> filter(is.na(.data[[col]]) | .data[[col]] >= input$need_min)
    if (!is.null(input$school) && input$school != "") {
      df <- df |> filter(cds == input$school)
    }
    df |> arrange(desc(.data[[col]]))
  })

  filtered_states <- reactive({
    df <- us_states
    if (!is_all_states()) {
      df <- df |> filter(state_abbr == input$state)
    }
    df |>
      filter(is.na(need_score_us) | need_score_us >= input$need_min) |>
      arrange(desc(need_score_us))
  })

  zip_map_data <- reactive({
    df <- filtered() |>
      filter(!is.na(zip), zip != "") |>
      group_by(zip) |>
      summarise(
        n_schools = n(),
        n_tested = sum(n_tested, na.rm = TRUE),
        pct_ela_all = mean(pct_ela_all, na.rm = TRUE),
        need = mean(.data[[need_col(input$district)]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      left_join(zip_centroids |> select(zip, lat, lng), by = "zip") |>
      filter(!is.na(lat), !is.na(lng))
    df
  })

  output$overview <- renderUI({
    if (is_ca()) {
      df <- filtered()
      n <- nrow(df)
      ela <- if (n == 0) NA_real_ else mean(df$pct_ela_all, na.rm = TRUE)
      mx <- if (n == 0) NA_real_ else max(df$economic_gap, na.rm = TRUE)
      place <- if (!is.null(input$district) && input$district != "") {
        input$district
      } else if (!is.null(input$county) && input$county != "") {
        paste(input$county, "County")
      } else {
        "California"
      }
      ca_naep <- us_states$pct_reading[us_states$state_abbr == "CA"][[1]]
      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "California CAASPP ELA met+",
            value = sprintf("%.1f%%", caaspp_state_pct),
            showcase = icon("flag"),
            theme = "primary",
            p("State test, 2024–25, all grades. Not the same as NAEP.")
          ),
          value_box(
            title = "California NAEP reading",
            value = sprintf("%.1f%%", ca_naep),
            showcase = icon("globe"),
            p("2024, grades 4 & 8 mean, at or above Proficient")
          ),
          value_box(
            title = paste("Schools in", place),
            value = format(n, big.mark = ","),
            showcase = icon("school"),
            p("CAASPP school-level file")
          ),
          value_box(
            title = "Largest economic gap in view",
            value = if (!is.finite(mx)) "—" else sprintf("%.1f pts", mx),
            showcase = icon("scale-unbalanced"),
            p("Not-SED minus SED on CAASPP ELA")
          )
        ),
        card(
          card_header("What this app does"),
          p("In California you can rank schools on CAASPP ELA gaps (income and English learners), map them by ZIP, and get a next step. Nationwide, states are ranked on NAEP reading — the comparable public measure.")
        )
      )
    } else {
      df <- filtered_states()
      n <- nrow(df)
      reading <- if (n == 0) NA_real_ else mean(df$pct_reading, na.rm = TRUE)
      mx <- if (n == 0) NA_real_ else max(df$ell_gap, na.rm = TRUE)
      place <- if (is_all_states()) "the United States" else df$state_name[[1]]
      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "National public NAEP reading",
            value = sprintf("%.1f%%", naep_nation),
            showcase = icon("flag"),
            theme = "primary",
            p("2024, grades 4 & 8 mean, at or above Proficient")
          ),
          value_box(
            title = paste("Average in", place),
            value = if (is.na(reading)) "—" else sprintf("%.1f%%", reading),
            showcase = icon("book-open"),
            p("Same NAEP metric")
          ),
          value_box(
            title = "States in view",
            value = format(n, big.mark = ","),
            showcase = icon("map"),
            p("50 states + D.C.")
          ),
          value_box(
            title = "Largest EL gap in view",
            value = if (!is.finite(mx)) "—" else sprintf("%.1f pts", mx),
            showcase = icon("scale-unbalanced"),
            p("Not ELL minus ELL, NAEP reading")
          )
        ),
        card(
          card_header("What this app does"),
          p(
            "NAEP is the only fully comparable public reading test across states. It cannot name schools. ",
            "Choose ",
            strong("California"),
            " for CAASPP school ZIP maps, or ",
            strong("Focus San Ramon Valley Unified"),
            " for the contest district."
          )
        )
      )
    }
  })

  output$table_header <- renderText({
    if (is_ca()) {
      col <- need_col(input$district)
      label <- if (col == "need_score_district") "district" else "California schools"
      paste0("Need ranking (", label, "; higher = larger literacy gaps)")
    } else {
      "Need ranking (US states on NAEP 2024 reading; higher = larger literacy gaps)"
    }
  })

  output$table <- renderDT({
    if (is_ca()) {
      col <- need_col(input$district)
      filtered() |>
        transmute(
          cds,
          Need = .data[[col]],
          School = school_name,
          District = district_name,
          County = county_name,
          ZIP = zip,
          `ELA % met+` = pct_ela_all,
          `vs CA` = gap_vs_state,
          `vs district` = gap_vs_district,
          `Economic gap` = economic_gap,
          `EL gap` = ell_gap,
          `N tested` = n_tested
        ) |>
        datatable(
          selection = "single",
          rownames = FALSE,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            scrollY = "55vh",
            order = list(list(1, "desc")),
            columnDefs = list(list(visible = FALSE, targets = 0))
          )
        ) |>
        formatRound(c("Need", "ELA % met+", "vs CA", "vs district", "Economic gap", "EL gap", "N tested"), 1)
    } else {
      filtered_states() |>
        transmute(
          state_abbr,
          Need = need_score_us,
          State = state_name,
          `Grade 4` = pct_grade4,
          `Grade 8` = pct_grade8,
          `NAEP reading` = pct_reading,
          `vs nation` = gap_vs_nation,
          `EL gap` = ell_gap
        ) |>
        datatable(
          selection = "single",
          rownames = FALSE,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            scrollY = "55vh",
            order = list(list(1, "desc")),
            columnDefs = list(list(visible = FALSE, targets = 0))
          )
        ) |>
        formatRound(c("Need", "Grade 4", "Grade 8", "NAEP reading", "vs nation", "EL gap"), 1)
    }
  })

  observeEvent(input$table_rows_selected, {
    idx <- input$table_rows_selected
    if (length(idx) != 1) {
      return()
    }
    if (is_ca()) {
      rows <- filtered()
      selected_cds(rows$cds[[idx]])
      updateSelectInput(session, "school", selected = rows$cds[[idx]])
    } else {
      rows <- filtered_states()
      selected_state_abbr(rows$state_abbr[[idx]])
    }
  })

  observeEvent(input$school, {
    if (!is.null(input$school) && input$school != "") {
      selected_cds(input$school)
    }
  })

  selected_school <- reactive({
    cds_id <- selected_cds()
    if (!is.null(cds_id) && cds_id %in% schools$cds) {
      return(schools[schools$cds == cds_id, , drop = FALSE])
    }
    filtered()[1, , drop = FALSE]
  })

  selected_state_row <- reactive({
    abbr <- selected_state_abbr()
    if (!is.null(abbr) && abbr %in% us_states$state_abbr) {
      return(us_states[us_states$state_abbr == abbr, , drop = FALSE])
    }
    filtered_states()[1, , drop = FALSE]
  })

  output$profile <- renderUI({
    tryCatch({
      if (is_ca()) {
        row <- selected_school()
        if (nrow(row) == 0 || is.na(row$school_name[[1]])) {
          return(tagList(
            h4("School profile"),
            p("No school matches the current filters. Pick a county/district or a row in Rankings.")
          ))
        }
        actions <- actions_for_school(row)
        return(tagList(
          h4(paste0(
            as.character(row$school_name[[1]]), " · ",
            as.character(row$district_name[[1]]), " · ",
            as.character(row$county_name[[1]]), " County"
          )),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(title = "CA need", value = need_label(row$need_score_ca), theme = "secondary"),
            value_box(title = "District need", value = need_label(row$need_score_district), theme = "secondary"),
            value_box(title = "ELA met or exceeded", value = pct_label(row$pct_ela_all), theme = "primary"),
            value_box(title = "Students with scores", value = count_label(row$n_tested))
          ),
          p(
            strong("Vs California: "), pts_label(row$gap_vs_state),
            " · ",
            strong("Vs district: "), pts_label(row$gap_vs_district),
            " · ZIP ", as.character(row$zip[[1]])
          ),
          h5("Literacy gaps inside this school"),
          tags$table(
            class = "table table-sm",
            tags$thead(tags$tr(tags$th("Group"), tags$th("ELA % met or exceeded"), tags$th("N"))),
            tags$tbody(
              tags$tr(tags$td("All students"), tags$td(pct_label(row$pct_ela_all)), tags$td(count_label(row$n_tested))),
              tags$tr(tags$td("Socioeconomically disadvantaged"), tags$td(pct_label(row$pct_ela_sed)), tags$td(count_label(row$n_sed_tested))),
              tags$tr(tags$td("Not socioeconomically disadvantaged"), tags$td(pct_label(row$pct_ela_not_sed)), tags$td("—")),
              tags$tr(tags$td("English learners"), tags$td(pct_label(row$pct_ela_ell)), tags$td(count_label(row$n_ell_tested))),
              tags$tr(tags$td("Fluent English / English-only"), tags$td(pct_label(row$pct_ela_fluent)), tags$td("—"))
            )
          ),
          p(
            strong("Economic gap: "), pts_label(row$economic_gap),
            " · ",
            strong("English learner gap: "), pts_label(row$ell_gap)
          ),
          h5("What to do next"),
          tags$ol(lapply(actions, tags$li)),
          p(
            a("CA School Dashboard", href = "https://www.caschooldashboard.org/", target = "_blank"),
            " · ",
            a("CAASPP results", href = "https://caaspp-elpac.ets.org/caaspp/", target = "_blank")
          )
        ))
      }

      row <- selected_state_row()
      if (nrow(row) == 0 || is.na(row$state_name[[1]])) {
        return(tagList(
          h4("State profile"),
          p("No state matches the current filters.")
        ))
      }
      actions <- actions_for_state(row)
      tagList(
        h4(paste0(as.character(row$state_name[[1]]), " · NAEP 2024 reading")),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(title = "US need", value = need_label(row$need_score_us), theme = "secondary"),
          value_box(title = "NAEP reading", value = pct_label(row$pct_reading), theme = "primary"),
          value_box(title = "Vs national public", value = pts_label(row$gap_vs_nation)),
          value_box(title = "EL gap", value = pts_label(row$ell_gap))
        ),
        tags$table(
          class = "table table-sm",
          tags$thead(tags$tr(tags$th("Group"), tags$th("% at or above NAEP Proficient"))),
          tags$tbody(
            tags$tr(tags$td("Grade 4, all students"), tags$td(pct_label(row$pct_grade4))),
            tags$tr(tags$td("Grade 8, all students"), tags$td(pct_label(row$pct_grade8))),
            tags$tr(tags$td("English learners (grades 4 & 8 mean)"), tags$td(pct_label(row$pct_ell))),
            tags$tr(tags$td("Not English learners"), tags$td(pct_label(row$pct_not_ell)))
          )
        ),
        p(class = "small text-muted", "NSLP/poverty subgroups are often missing in recent NAEP files, so state need uses overall reading, the EL gap, and the gap versus national public schools."),
        h5("What to do next"),
        tags$ol(lapply(actions, tags$li)),
        p(
          a("NAEP state snapshots", href = "https://www.nationsreportcard.gov/profiles/stateprofile", target = "_blank"),
          " · ",
          a("The Nation’s Report Card", href = "https://www.nationsreportcard.gov/", target = "_blank")
        )
      )
    }, error = function(e) {
      tagList(h4("Profile error"), p(conditionMessage(e)))
    })
  })

  output$zip_map <- renderLeaflet({
    # Render only once the Map tab is active so the container has a real size.
    req(is.null(input$main_nav) || identical(input$main_nav, "Map"))
    pal <- colorNumeric("YlOrRd", domain = c(0, 100), na.color = "#cccccc")
    if (is_ca()) {
      df <- zip_map_data()
      map <- leaflet(df) |> addTiles()
      if (nrow(df) == 0) {
        return(map |> fitBounds(-124.5, 32.5, -114.1, 42.1))
      }
      return(
        map |>
          addCircleMarkers(
            lng = ~lng,
            lat = ~lat,
            layerId = ~paste0("zip:", zip),
            radius = ~pmax(5, pmin(16, sqrt(n_schools) * 3.5)),
            color = "#7b241c",
            weight = 1,
            fillColor = ~pal(need),
            fillOpacity = 0.8,
            popup = ~paste0(
              "<strong>ZIP ", zip, "</strong><br/>",
              n_schools, " school(s)<br/>",
              "Avg ELA met+: ", sprintf("%.1f%%", pct_ela_all), "<br/>",
              "Avg need: ", sprintf("%.0f", need)
            )
          ) |>
          fitBounds(
            lng1 = min(df$lng, na.rm = TRUE) - 0.3,
            lat1 = min(df$lat, na.rm = TRUE) - 0.3,
            lng2 = max(df$lng, na.rm = TRUE) + 0.3,
            lat2 = max(df$lat, na.rm = TRUE) + 0.3
          ) |>
          addLegend("bottomright", pal = pal, values = ~need, title = "Need", opacity = 0.8)
      )
    }

    df <- filtered_states()
    r <- if (is_all_states()) 12 else 18
    # Use default OSM tiles — CartoDB.Positron now requires an API key.
    map <- leaflet(df) |> addTiles()
    if (nrow(df) == 0) {
      return(map |> setView(lng = -96.5, lat = 39.5, zoom = 4))
    }
    # Pin a continental-US view so AK/HI don't zoom the map out to "empty".
    view_map <- if (is_all_states() || nrow(df) > 5) {
      map |> setView(lng = -96.5, lat = 39.5, zoom = 4)
    } else {
      map |> setView(lng = mean(df$lng), lat = mean(df$lat), zoom = 5)
    }
    view_map |>
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        layerId = ~paste0("st:", state_abbr),
        radius = r,
        color = "#7b241c",
        weight = 1,
        fillColor = ~pal(need_score_us),
        fillOpacity = 0.85,
        label = ~state_abbr,
        popup = ~paste0(
          "<strong>", state_name, "</strong><br/>",
          "NAEP reading: ", sprintf("%.1f%%", pct_reading), "<br/>",
          "Need: ", sprintf("%.0f", need_score_us)
        )
      ) |>
      addLegend("bottomright", pal = pal, values = ~need_score_us, title = "Need", opacity = 0.8)
  })

  observeEvent(input$zip_map_marker_click, {
    click <- input$zip_map_marker_click
    if (is.null(click$id)) {
      return()
    }
    if (startsWith(click$id, "zip:")) {
      z <- sub("^zip:", "", click$id)
      hit <- schools[schools$zip == z, , drop = FALSE]
      if (nrow(hit) == 0) {
        return()
      }
      pending_zip(z)
      updateSelectInput(session, "state", selected = "CA")
      updateSelectInput(session, "county", selected = hit$county_name[[1]])
    } else if (startsWith(click$id, "st:")) {
      abbr <- sub("^st:", "", click$id)
      selected_state_abbr(abbr)
      updateSelectInput(session, "state", selected = abbr)
    }
  })

  output$plot_econ <- renderPlot({
    if (is_ca()) {
      df <- filtered() |> filter(!is.na(pct_ela_all), !is.na(economic_gap))
      if (nrow(df) == 0) {
        return(ggplot() + theme_void() + ggtitle("No schools with both ELA and economic-gap values"))
      }
      return(
        ggplot(df, aes(economic_gap, pct_ela_all)) +
          geom_point(aes(size = n_tested), alpha = 0.55, color = "#1b4f72") +
          geom_hline(yintercept = caaspp_state_pct, linetype = 2, color = "gray40") +
          labs(x = "Economic gap (percentage points)", y = "ELA % met or exceeded", size = "N tested") +
          theme_minimal(base_size = 13)
      )
    }
    df <- filtered_states() |> filter(!is.na(pct_reading), !is.na(gap_vs_nation))
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() + ggtitle("No state NAEP values"))
    }
    ggplot(df, aes(gap_vs_nation, pct_reading, label = state_abbr)) +
      geom_point(size = 3, alpha = 0.75, color = "#1b4f72") +
      geom_hline(yintercept = naep_nation, linetype = 2, color = "gray40") +
      labs(x = "Gap vs national public (percentage points)", y = "NAEP reading % proficient") +
      theme_minimal(base_size = 13)
  })

  output$plot_ell <- renderPlot({
    if (is_ca()) {
      df <- filtered() |> filter(!is.na(pct_ela_all), !is.na(ell_gap))
      if (nrow(df) == 0) {
        return(ggplot() + theme_void() + ggtitle("No schools with both ELA and English-learner-gap values"))
      }
      return(
        ggplot(df, aes(ell_gap, pct_ela_all)) +
          geom_point(aes(size = n_tested), alpha = 0.55, color = "#117a65") +
          geom_hline(yintercept = caaspp_state_pct, linetype = 2, color = "gray40") +
          labs(x = "English learner gap (percentage points)", y = "ELA % met or exceeded", size = "N tested") +
          theme_minimal(base_size = 13)
      )
    }
    df <- filtered_states() |> filter(!is.na(pct_reading), !is.na(ell_gap))
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() + ggtitle("No state English-learner-gap values"))
    }
    ggplot(df, aes(ell_gap, pct_reading)) +
      geom_point(size = 3, alpha = 0.75, color = "#117a65") +
      geom_hline(yintercept = naep_nation, linetype = 2, color = "gray40") +
      labs(x = "English learner gap (percentage points)", y = "NAEP reading % proficient") +
      theme_minimal(base_size = 13)
  })

  school_pick_choices <- reactive({
    df <- geo()
    if (nrow(df) > 800) {
      df <- df |> arrange(desc(need_score_ca)) |> slice_head(n = 800)
    }
    named_choices(df$cds, paste0(df$school_name, " (", df$district_name, ")"))
  })

  output$compare_controls <- renderUI({
    if (is_ca()) {
      ch <- school_pick_choices()
      def_a <- if (!is.null(selected_cds()) && selected_cds() %in% ch) selected_cds() else unname(ch[[1]])
      def_b <- if (length(ch) >= 2) unname(ch[[2]]) else def_a
      tagList(
        radioButtons(
          "compare_mode",
          "Compare",
          choices = c("Two schools" = "schools", "Two years (2024–25 vs 2023–24)" = "years"),
          selected = isolate(input$compare_mode) %||% "schools",
          inline = TRUE
        ),
        selectizeInput("compare_a", "School A", choices = ch, selected = def_a, width = "100%"),
        conditionalPanel(
          "input.compare_mode == 'schools'",
          selectizeInput("compare_b", "School B", choices = ch, selected = def_b, width = "100%")
        ),
        p(class = "small text-muted", "Lists the current county/district filter (capped at 800 schools). SED vs not-SED and EL vs fluent come from the public CAASPP file; small groups are blank.")
      )
    } else {
      ch <- setNames(us_states$state_abbr, us_states$state_name)
      def_a <- selected_state_abbr() %||% "CA"
      def_b <- if (identical(def_a, "CA")) "TX" else "CA"
      tagList(
        radioButtons(
          "compare_mode",
          "Compare",
          choices = c("Two states" = "schools", "Two years (NAEP 2024 vs 2022)" = "years"),
          selected = isolate(input$compare_mode) %||% "schools",
          inline = TRUE
        ),
        selectInput("compare_state_a", "State A", choices = ch, selected = def_a),
        conditionalPanel(
          "input.compare_mode == 'schools'",
          selectInput("compare_state_b", "State B", choices = ch, selected = def_b)
        ),
        p(class = "small text-muted", "NAEP reports ELL vs not ELL. NSLP/poverty is often missing in 2024, so income gaps are not shown for states.")
      )
    }
  })

  compare_bundle <- reactive({
    mode <- input$compare_mode %||% "schools"
    if (is_ca()) {
      a_id <- input$compare_a
      if (is.null(a_id) || !nzchar(a_id)) {
        return(NULL)
      }
      a <- schools[schools$cds == a_id, , drop = FALSE]
      if (nrow(a) == 0) {
        return(NULL)
      }
      if (identical(mode, "years")) {
        b <- schools_prior[schools_prior$cds == a_id, , drop = FALSE]
        if (nrow(b) == 0) {
          return(list(error = "No 2023–24 CAASPP row for this CDS code (new school or suppressed)."))
        }
        return(list(
          kind = "ca",
          a_lab = paste0(a$school_name, " · 2024–25"),
          b_lab = paste0(b$school_name, " · 2023–24"),
          a = a,
          b = b,
          metrics = c("pct_ela_all", "pct_ela_sed", "pct_ela_not_sed", "pct_ela_ell", "pct_ela_fluent", "economic_gap", "ell_gap")
        ))
      }
      b_id <- input$compare_b
      if (is.null(b_id) || !nzchar(b_id)) {
        return(NULL)
      }
      b <- schools[schools$cds == b_id, , drop = FALSE]
      if (nrow(b) == 0) {
        return(NULL)
      }
      return(list(
        kind = "ca",
        a_lab = as.character(a$school_name[[1]]),
        b_lab = as.character(b$school_name[[1]]),
        a = a,
        b = b,
        metrics = c("pct_ela_all", "pct_ela_sed", "pct_ela_not_sed", "pct_ela_ell", "pct_ela_fluent", "economic_gap", "ell_gap")
      ))
    }
    a_id <- input$compare_state_a
    if (is.null(a_id) || !nzchar(a_id)) {
      return(NULL)
    }
    a <- us_states[us_states$state_abbr == a_id, , drop = FALSE]
    if (nrow(a) == 0) {
      return(NULL)
    }
    if (identical(mode, "years")) {
      plot_df <- data.frame(
        unit = c("2022", "2024", "2022", "2024"),
        metric = c("Reading", "Reading", "EL gap", "EL gap"),
        value = c(a$pct_reading_2022, a$pct_reading, a$ell_gap_2022, a$ell_gap)
      )
      tab <- data.frame(
        Metric = c("NAEP reading (gr. 4 & 8)", "EL gap (not ELL minus ELL)"),
        `2022` = c(a$pct_reading_2022, a$ell_gap_2022),
        `2024` = c(a$pct_reading, a$ell_gap),
        Change = c(a$change_vs_2022, ifelse(is.na(a$ell_gap) | is.na(a$ell_gap_2022), NA, a$ell_gap - a$ell_gap_2022)),
        check.names = FALSE
      )
      return(list(kind = "us_years", plot_df = plot_df, tab = tab, title = paste(a$state_name, "NAEP 2022 vs 2024")))
    }
    b_id <- input$compare_state_b
    if (is.null(b_id) || !nzchar(b_id)) {
      return(NULL)
    }
    b <- us_states[us_states$state_abbr == b_id, , drop = FALSE]
    if (nrow(b) == 0) {
      return(NULL)
    }
    list(
      kind = "us",
      a_lab = as.character(a$state_name[[1]]),
      b_lab = as.character(b$state_name[[1]]),
      a = a,
      b = b
    )
  })

  output$compare_plot <- renderPlot({
    bun <- compare_bundle()
    if (is.null(bun)) {
      return(ggplot() + theme_void() + ggtitle("Pick two places to compare"))
    }
    if (!is.null(bun$error)) {
      return(ggplot() + theme_void() + ggtitle(bun$error))
    }
    if (identical(bun$kind, "us_years")) {
      df <- bun$plot_df |> filter(!is.na(value))
      return(
        ggplot(df, aes(metric, value, fill = unit)) +
          geom_col(position = "dodge") +
          labs(x = NULL, y = "Percent / points", fill = NULL, title = bun$title) +
          theme_minimal(base_size = 13)
      )
    }
    if (identical(bun$kind, "ca")) {
      metric <- c("All", "SED", "Not SED", "EL", "Fluent")
      df <- data.frame(
        unit = rep(c(bun$a_lab, bun$b_lab), each = 5),
        metric = rep(metric, 2),
        value = c(
          bun$a$pct_ela_all, bun$a$pct_ela_sed, bun$a$pct_ela_not_sed, bun$a$pct_ela_ell, bun$a$pct_ela_fluent,
          bun$b$pct_ela_all, bun$b$pct_ela_sed, bun$b$pct_ela_not_sed, bun$b$pct_ela_ell, bun$b$pct_ela_fluent
        )
      )
      ylab <- "CAASPP ELA % met or exceeded"
    } else {
      metric <- c("Reading", "Grade 4", "Grade 8", "ELL", "Not ELL")
      df <- data.frame(
        unit = rep(c(bun$a_lab, bun$b_lab), each = 5),
        metric = rep(metric, 2),
        value = c(
          bun$a$pct_reading, bun$a$pct_grade4, bun$a$pct_grade8, bun$a$pct_ell, bun$a$pct_not_ell,
          bun$b$pct_reading, bun$b$pct_grade4, bun$b$pct_grade8, bun$b$pct_ell, bun$b$pct_not_ell
        )
      )
      ylab <- "NAEP % at or above Proficient"
    }
    df <- df |> filter(!is.na(value))
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() + ggtitle("No overlapping public subgroup values"))
    }
    ggplot(df, aes(metric, value, fill = unit)) +
      geom_col(position = "dodge") +
      labs(x = NULL, y = ylab, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$compare_table <- renderDT({
    bun <- compare_bundle()
    empty <- datatable(data.frame(Note = "Pick two places"), options = list(dom = "t"), rownames = FALSE)
    if (is.null(bun)) {
      return(empty)
    }
    if (!is.null(bun$error)) {
      return(datatable(data.frame(Note = bun$error), options = list(dom = "t"), rownames = FALSE))
    }
    if (identical(bun$kind, "us_years")) {
      return(datatable(bun$tab, options = list(dom = "t"), rownames = FALSE))
    }
    if (identical(bun$kind, "ca")) {
      tab <- data.frame(
        Metric = c("All students", "Economically disadvantaged (SED)", "Not SED", "English learners", "Fluent / English-only", "Economic gap (pts)", "EL gap (pts)"),
        A = c(bun$a$pct_ela_all, bun$a$pct_ela_sed, bun$a$pct_ela_not_sed, bun$a$pct_ela_ell, bun$a$pct_ela_fluent, bun$a$economic_gap, bun$a$ell_gap),
        B = c(bun$b$pct_ela_all, bun$b$pct_ela_sed, bun$b$pct_ela_not_sed, bun$b$pct_ela_ell, bun$b$pct_ela_fluent, bun$b$economic_gap, bun$b$ell_gap)
      )
      names(tab) <- c("Metric", bun$a_lab, bun$b_lab)
    } else {
      tab <- data.frame(
        Metric = c("NAEP reading", "Grade 4", "Grade 8", "ELL", "Not ELL", "EL gap (pts)", "Vs nation (pts)"),
        A = c(bun$a$pct_reading, bun$a$pct_grade4, bun$a$pct_grade8, bun$a$pct_ell, bun$a$pct_not_ell, bun$a$ell_gap, bun$a$gap_vs_nation),
        B = c(bun$b$pct_reading, bun$b$pct_grade4, bun$b$pct_grade8, bun$b$pct_ell, bun$b$pct_not_ell, bun$b$ell_gap, bun$b$gap_vs_nation)
      )
      names(tab) <- c("Metric", bun$a_lab, bun$b_lab)
    }
    datatable(tab, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })

  output$profile_subgroups <- renderPlot({
    # Avoid drawing at 0×0 while another tab is active (causes "invalid 'height'").
    req(is.null(input$main_nav) || identical(input$main_nav, "Profile"))
    empty_plot <- function(title) {
      ggplot() + theme_void() + ggtitle(title)
    }
    if (is_ca()) {
      row <- selected_school()
      if (nrow(row) == 0) {
        return(empty_plot("No school selected"))
      }
      df <- data.frame(
        group = c("All", "SED", "Not SED", "EL", "Fluent"),
        value = as.numeric(c(row$pct_ela_all, row$pct_ela_sed, row$pct_ela_not_sed, row$pct_ela_ell, row$pct_ela_fluent))
      ) |> filter(!is.na(value))
      if (nrow(df) == 0) {
        return(empty_plot("No public subgroup values"))
      }
      return(
        ggplot(df, aes(group, value, fill = group)) +
          geom_col(fill = "#1b4f72") +
          coord_cartesian(ylim = c(0, 100)) +
          guides(fill = "none") +
          labs(x = NULL, y = "CAASPP ELA % met or exceeded", title = "Subgroup gaps (public file)") +
          theme_minimal(base_size = 13)
      )
    }
    row <- selected_state_row()
    if (nrow(row) == 0) {
      return(empty_plot("No state selected"))
    }
    df <- data.frame(
      group = c("All (4&8)", "ELL", "Not ELL"),
      value = as.numeric(c(row$pct_reading, row$pct_ell, row$pct_not_ell))
    ) |> filter(!is.na(value))
    if (nrow(df) == 0) {
      return(empty_plot("No public subgroup values"))
    }
    ggplot(df, aes(group, value, fill = group)) +
      geom_col(fill = "#1b4f72") +
      coord_cartesian(ylim = c(0, 100)) +
      guides(fill = "none") +
      labs(x = NULL, y = "NAEP % proficient", title = "English learner gap (NAEP)") +
      theme_minimal(base_size = 13)
  }, width = 720, height = 300)

  briefing_lines <- reactive({
    if (is_ca()) {
      row <- selected_school()
      if (nrow(row) == 0) {
        return("Select a California school first.")
      }
      return(briefing_for_school(row))
    }
    row <- selected_state_row()
    if (nrow(row) == 0) {
      return("Select a state first.")
    }
    briefing_for_state(row)
  })

  output$briefing_preview <- renderText({
    paste(briefing_lines(), collapse = "\n")
  })

  output$briefing_pdf <- downloadHandler(
    filename = function() "literacy-briefing.pdf",
    content = function(file) {
      write_briefing_pdf(file, briefing_lines())
    }
  )

  output$briefing_csv <- downloadHandler(
    filename = function() "literacy-briefing.csv",
    content = function(file) {
      write.csv(data.frame(line = briefing_lines()), file, row.names = FALSE)
    }
  )

  classroom_scored <- reactive({
    path <- input$classroom_file$datapath
    raw <- if (is.null(path)) classroom_template else read_csv(path, na = c("", "NA"))
    score_classroom(raw, caaspp_state_pct)
  })

  output$classroom_template_dl <- downloadHandler(
    filename = function() "classroom_template.csv",
    content = function(file) {
      write.csv(classroom_template, file, row.names = FALSE)
    }
  )

  output$classroom_table <- renderDT({
    df <- tryCatch(classroom_scored(), error = function(e) data.frame(error = e$message))
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })

  output$classroom_plot <- renderPlot({
    df <- tryCatch(classroom_scored(), error = function(e) NULL)
    if (is.null(df) || !"pct_ela_all" %in% names(df)) {
      return(ggplot() + theme_void() + ggtitle("Upload a CSV with the template columns"))
    }
    ggplot(df, aes(school_name, pct_ela_all)) +
      geom_col(fill = "#1b4f72") +
      geom_hline(yintercept = caaspp_state_pct, linetype = 2) +
      labs(x = NULL, y = "ELA % met or exceeded", title = sprintf("Dashed line = California CAASPP ELA (%.1f%%)", caaspp_state_pct)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })
}

shinyApp(ui, server)
