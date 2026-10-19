# Need scores (must match scripts/prepare_data.py and scripts/prepare_us_states.py).
#
# California schools (CAASPP):
# 50% low overall ELA proficiency
# 25% economic gap (not-SED minus SED)
# 15% English learner gap (fluent minus EL)
# 10% gap versus California or district average
#
# US states (NAEP 2024 reading, grades 4 and 8):
# 60% low share at or above NAEP Proficient
# 25% English learner gap
# 15% gap versus national public
#
# Rankings are relative, not a prediction of future scores.

need_formula_html <- function() {
  paste(
    "<p><strong>California school need (0–100)</strong> uses 2024–25 CAASPP ELA",
    "(all grades combined) and ranks schools against other California schools or",
    "against their district.</p>",
    "<ul>",
    "<li>50% overall share meeting or exceeding ELA standards (lower = higher need)</li>",
    "<li>25% economic gap: not socioeconomically disadvantaged minus SED</li>",
    "<li>15% English learner gap: fluent English / English-only minus EL</li>",
    "<li>10% gap versus the California average (statewide ranking) or the district average (district ranking)</li>",
    "</ul>",
    "<p><strong>United States state need (0–100)</strong> uses NAEP 2024 reading,",
    "grades 4 and 8, percent at or above Proficient — the only fully comparable",
    "public reading measure across states. School-level state tests are not comparable</p>",
    "<ul>",
    "<li>60% overall NAEP reading (mean of grades 4 and 8; lower = higher need)</li>",
    "<li>25% English learner gap (not ELL minus ELL)</li>",
    "<li>15% gap versus national public schools</li>",
    "</ul>",
    "<p>Missing subgroup rows are skipped and remaining weights are renormalized.",
    "Association is not causation. NAEP cannot identify individual schools.</p>"
  )
}

actions_for_school <- function(row) {
  items <- character()
  ela <- suppressWarnings(as.numeric(row$pct_ela_all))
  econ <- suppressWarnings(as.numeric(row$economic_gap))
  ell <- suppressWarnings(as.numeric(row$ell_gap))
  vs_d <- suppressWarnings(as.numeric(row$gap_vs_district))
  vs_s <- suppressWarnings(as.numeric(row$gap_vs_state))
  county <- as.character(row$county_name[[1]])
  district <- as.character(row$district_name[[1]])

  if (!is.na(ela) && ela < 70) {
    items <- c(items, "Prioritize a site literacy plan: daily structured literacy blocks and progress monitoring (DIBELS/STAR or district equivalent).")
  }
  if (!is.na(econ) && econ >= 25) {
    items <- c(items, "Economic gap is large. Expand high-dosage tutoring and make family literacy nights free, with meals and translation.")
  }
  if (!is.na(ell) && ell >= 40) {
    items <- c(items, "English learner gap is large. Strengthen designated ELD, bilingual books, and teacher coaching for integrated ELD in ELA.")
  }
  if (!is.na(vs_d) && vs_d < -5) {
    items <- c(items, paste0("This school sits below the ", district, " ELA average. Ask for the School Plan for Student Achievement (SPSA) literacy actions and Title I/LCAP dollars tied to reading."))
  }
  if (!is.na(vs_s) && vs_s < -10) {
    items <- c(items, "ELA is below the California average. Compare the CA School Dashboard English Language Arts indicator and request extra reading support.")
  }
  if (grepl("Continuation|Alternative", row$school_name, ignore.case = TRUE)) {
    items <- c(items, "Continuation/alternative programs often have smaller tested groups. Pair CAASPP with credit recovery and credit-bearing literacy, not only state-test rankings.")
  }
  library_note <- if (identical(county, "Contra Costa")) {
    "Point families to Contra Costa County Library (homework help, cards, and summer reading)."
  } else {
    paste0("Point families to the public library system serving ", county, " County for cards, homework help, and summer reading.")
  }
  items <- c(
    items,
    "Share this profile with the school site council and request the latest CA School Dashboard English Language Arts indicator.",
    library_note
  )
  unique(items)
}

actions_for_state <- function(row) {
  items <- character()
  reading <- suppressWarnings(as.numeric(row$pct_reading))
  ell <- suppressWarnings(as.numeric(row$ell_gap))
  vs <- suppressWarnings(as.numeric(row$gap_vs_nation))
  name <- as.character(row$state_name[[1]])

  if (!is.na(reading) && reading < 30) {
    items <- c(items, paste0(name, " is below 30% at or above NAEP Proficient in reading (grades 4 and 8 combined). Ask the state education agency for its evidence-based literacy plan and how Title I and literacy grants are targeted."))
  }
  if (!is.na(ell) && ell >= 20) {
    items <- c(items, "The NAEP English learner gap is large. Look for statewide ELD standards, bilingual book access, and teacher training for integrated English language development.")
  }
  if (!is.na(vs) && vs < -3) {
    items <- c(items, paste0(name, " sits below the national public NAEP reading average. Compare the Nation’s Report Card state snapshot with neighboring states, not with that state’s own % proficient on a local test."))
  }
  items <- c(
    items,
    "NAEP is a sample of public schools — it cannot name a campus. Use your state report card or, in California, this app’s CAASPP school list.",
    "Share the state NAEP snapshot with a congressional office when discussing literacy funding."
  )
  unique(items)
}
