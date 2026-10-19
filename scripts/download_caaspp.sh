#!/bin/sh
# Download California CAASPP ELA research files and ZIP centroids.
set -e
cd "$(dirname "$0")/.."
mkdir -p data/raw
cd data/raw
curl -fsSL -o sb_ca2025_all_csv_ela_v1.zip "https://caaspp-elpac.ets.org/caaspp/researchfiles/sb_ca2025_all_csv_ela_v1.zip"
curl -fsSL -o sb_ca2025entities_csv.zip "https://caaspp-elpac.ets.org/caaspp/researchfiles/sb_ca2025entities_csv.zip"
curl -fsSL -o 2024_Gaz_zcta_national.zip "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_zcta_national.zip"
unzip -o sb_ca2025_all_csv_ela_v1.zip
unzip -o sb_ca2025entities_csv.zip
unzip -o 2024_Gaz_zcta_national.zip
echo "Done. Run: python3 scripts/prepare_data.py"
