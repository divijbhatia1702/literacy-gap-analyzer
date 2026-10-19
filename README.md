# Literacy Gap Analyzer

Shiny app for the **2026 Congressional App Challenge** (California’s **10th District**). It ranks **US states on NAEP reading** and **California schools on CAASPP ELA**, with maps and next steps.

Contest home district: **San Ramon Valley Unified**, CDS **07-61804**, Contra Costa County.

## Features

- **All 50 states + D.C.:** NAEP 2024 reading, grades 4 and 8, percent at or above Proficient (comparable across states).
- **California schools:** **9,431** public schools, **2024–25** CAASPP ELA, ZIP map, SED and English-learner gaps.
- State need score (NAEP) and California school / district need scores (CAASPP).
- **Compare:** two California schools, two years of CAASPP (2024–25 vs 2023–24), two states, or NAEP 2024 vs 2022.
- **Subgroup gaps:** SED vs not SED and English learners vs fluent (CAASPP); ELL vs not ELL (NAEP). Income is often missing on recent NAEP.
- **Staffer briefing:** one-page PDF or CSV from the selected school or state.
- **Classroom demo:** upload a CSV (template included) and rank rooms against California’s CAASPP ELA average.

Processed tables:

- `data/processed/us_states.csv`
- `data/processed/ca_schools.csv`
- `data/processed/ca_schools_prior.csv`
- `data/processed/ca_zips.csv`
- `data/processed/srvusd_schools.csv`
- `data/processed/classroom_template.csv`

## Rebuild data

```bash
python3 scripts/prepare_us_states.py
bash scripts/download_caaspp.sh
python3 scripts/prepare_data.py
python3 scripts/prepare_trends.py
```

Sources:

- [NAEP Data Service API](https://www.nationsreportcard.gov/api_documentation.aspx)
- [CAASPP research files](https://caaspp-elpac.ets.org/caaspp/ResearchFileListSB?lstTestYear=2025&ps=true)
- [Census 2024 ZCTA gazetteer](https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_zcta_national.zip)

## Run locally

```bash
# From this folder, with R packages: shiny bslib dplyr ggplot2 DT leaflet readr
Rscript -e 'shiny::runApp("app.R", host = "0.0.0.0", port = 4789)'
```

Open http://127.0.0.1:4789

