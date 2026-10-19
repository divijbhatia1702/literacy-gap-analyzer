# One-page briefing text and classroom CSV scoring.

fmt_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (length(x) == 0 || is.na(x)) return("n/a")
  sprintf("%.1f%%", x)
}

fmt_pts <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (length(x) == 0 || is.na(x)) return("n/a")
  sprintf("%.1f pts", x)
}

briefing_for_school <- function(row) {
  name <- as.character(row$school_name[[1]])
  district <- as.character(row$district_name[[1]])
  county <- as.character(row$county_name[[1]])
  actions <- actions_for_school(row)
  c(
    "Literacy Gap Analyzer — one-page briefing (staffer)",
    "Contest: Congressional App Challenge 2026, CA-10",
    "",
    paste0("School: ", name),
    paste0("District: ", district),
    paste0("County: ", county),
    paste0("CDS: ", as.character(row$cds[[1]])),
    paste0("ZIP: ", as.character(row$zip[[1]])),
    "Source: 2024–25 CAASPP ELA (all grades combined). Not NAEP.",
    "",
    paste0("ELA met or exceeded: ", fmt_pct(row$pct_ela_all), " (n tested ", as.character(row$n_tested[[1]]), ")"),
    paste0("Economically disadvantaged (SED): ", fmt_pct(row$pct_ela_sed)),
    paste0("Not SED: ", fmt_pct(row$pct_ela_not_sed)),
    paste0("Economic gap (not SED minus SED): ", fmt_pts(row$economic_gap)),
    paste0("English learners: ", fmt_pct(row$pct_ela_ell)),
    paste0("Fluent / English only: ", fmt_pct(row$pct_ela_fluent)),
    paste0("EL gap (fluent minus EL): ", fmt_pts(row$ell_gap)),
    paste0("Vs district average: ", fmt_pts(row$gap_vs_district)),
    paste0("Vs California average: ", fmt_pts(row$gap_vs_state)),
    paste0("School need score: ", sprintf("%.0f/100", suppressWarnings(as.numeric(row$need_score_ca[[1]])))),
    "",
    "Suggested next steps:",
    paste0("- ", actions),
    "",
    "Caveats: public file suppresses small groups. Rankings are relative, not a forecast."
  )
}

briefing_for_state <- function(row) {
  name <- as.character(row$state_name[[1]])
  actions <- actions_for_state(row)
  c(
    "Literacy Gap Analyzer — one-page briefing (staffer)",
    "Contest: Congressional App Challenge 2026, CA-10",
    "",
    paste0("Jurisdiction: ", name, " (", as.character(row$state_abbr[[1]]), ")"),
    "Source: NAEP 2024 reading, grades 4 and 8, percent at or above Proficient.",
    "NAEP cannot name individual schools. Do not mix with CAASPP met-and-above.",
    "",
    paste0("NAEP reading (grades 4 & 8 mean): ", fmt_pct(row$pct_reading)),
    paste0("Grade 4: ", fmt_pct(row$pct_grade4), "   Grade 8: ", fmt_pct(row$pct_grade8)),
    paste0("English learners: ", fmt_pct(row$pct_ell)),
    paste0("Not ELL: ", fmt_pct(row$pct_not_ell)),
    paste0("EL gap (not ELL minus ELL): ", fmt_pts(row$ell_gap)),
    paste0("Vs national public: ", fmt_pts(row$gap_vs_nation)),
    paste0("NAEP 2022 reading (same grades): ", fmt_pct(row$pct_reading_2022)),
    paste0("Change vs 2022: ", fmt_pts(row$change_vs_2022)),
    paste0("State need score: ", sprintf("%.0f/100", suppressWarnings(as.numeric(row$need_score_us[[1]])))),
    "",
    "Suggested next steps:",
    paste0("- ", actions),
    "",
    "Caveats: NAEP is a sample of public schools. Association is not causation."
  )
}

write_briefing_pdf <- function(path, lines) {
  grDevices::pdf(path, width = 8.5, height = 11, title = "Literacy briefing")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0.6, 0.8, 0.6, 0.6), xpd = NA)
  graphics::plot.new()
  graphics::text(
    0, 1, paste(lines, collapse = "\n"),
    adj = c(0, 1), cex = 0.78, family = "sans"
  )
}

rescale01 <- function(x, invert = FALSE) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[[1]]) || rng[[1]] == rng[[2]]) {
    return(rep(NA_real_, length(x)))
  }
  out <- (x - rng[[1]]) / (rng[[2]] - rng[[1]]) * 100
  if (invert) out <- 100 - out
  out
}

score_classroom <- function(df, state_pct) {
  req_cols <- c(
    "school_name", "n_tested", "pct_ela_all",
    "pct_ela_sed", "pct_ela_not_sed", "pct_ela_ell", "pct_ela_fluent"
  )
  missing <- setdiff(req_cols, names(df))
  if (length(missing) > 0) {
    stop("CSV needs columns: ", paste(req_cols, collapse = ", "))
  }
  df <- df[, req_cols, drop = FALSE]
  df <- df |>
    mutate(
      across(-school_name, ~ suppressWarnings(as.numeric(.x))),
      economic_gap = pct_ela_not_sed - pct_ela_sed,
      ell_gap = pct_ela_fluent - pct_ela_ell,
      gap_vs_state = round(pct_ela_all - state_pct, 2)
    )
  w_overall <- 0.5
  w_econ <- 0.25
  w_ell <- 0.15
  w_vs <- 0.1
  overall_need <- rescale01(df$pct_ela_all, invert = TRUE)
  econ_need <- rescale01(df$economic_gap)
  ell_need <- rescale01(df$ell_gap)
  vs_need <- rescale01(df$gap_vs_state, invert = TRUE)
  present <- function(x) as.numeric(!is.na(x))
  num <- w_overall * overall_need + w_econ * econ_need + w_ell * ell_need + w_vs * vs_need
  den <- w_overall * present(overall_need) + w_econ * present(econ_need) +
    w_ell * present(ell_need) + w_vs * present(vs_need)
  df$need_score <- ifelse(den > 0, round(num / den, 1), NA_real_)
  df
}
