"""Build prior-year CAASPP school metrics for year-over-year compare."""

from __future__ import annotations

import importlib.util
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "data" / "raw"
PROCESSED = ROOT / "data" / "processed"
PRIOR = RAW / "sb_ca2024_all_csv_ela_v1.txt"


def load_prepare():
    path = ROOT / "scripts" / "prepare_data.py"
    spec = importlib.util.spec_from_file_location("prepare_data", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def main() -> None:
    if not PRIOR.exists():
        raise FileNotFoundError(f"Missing {PRIOR}. Download sb_ca2024_all_csv_ela_v1.zip")
    mod = load_prepare()
    entities, county_names, district_names = mod.load_entities()
    scores, district_pct, county_pct, state_pct = mod.stream_aggregates(PRIOR)

    recs = []
    for sid, rec in scores.items():
        if rec.get("pct_ela_all") is None:
            continue
        meta = entities.get(sid, {})
        county = rec["county_code"]
        dkey = mod.school_id(county, rec["district_code"], "0000000")
        rec["county_name"] = meta.get("county_name") or county_names.get(county, "")
        rec["district_name"] = rec["district_name"] or meta.get("district_name") or district_names.get(dkey, "")
        rec["school_name"] = rec["school_name"] or meta.get("school_name", "")
        rec["cds"] = sid.replace("-", "")
        rec["test_year"] = rec.get("test_year") or "2024"
        rec["economic_gap"] = (
            None
            if rec["pct_ela_sed"] is None or rec["pct_ela_not_sed"] is None
            else round(rec["pct_ela_not_sed"] - rec["pct_ela_sed"], 2)
        )
        rec["ell_gap"] = (
            None
            if rec["pct_ela_ell"] is None or rec["pct_ela_fluent"] is None
            else round(rec["pct_ela_fluent"] - rec["pct_ela_ell"], 2)
        )
        rec["state_pct_ela"] = state_pct
        recs.append(rec)

    fields = [
        "cds",
        "school_name",
        "district_name",
        "county_name",
        "test_year",
        "n_tested",
        "pct_ela_all",
        "pct_ela_sed",
        "pct_ela_not_sed",
        "pct_ela_ell",
        "pct_ela_fluent",
        "economic_gap",
        "ell_gap",
        "state_pct_ela",
    ]
    out = PROCESSED / "ca_schools_prior.csv"
    PROCESSED.mkdir(parents=True, exist_ok=True)
    mod.write_csv(out, recs, fields)
    print(f"Wrote {len(recs)} prior-year schools to {out}; statewide ELA {state_pct}")


if __name__ == "__main__":
    main()
