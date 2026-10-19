"""Build NAEP 2024 state reading table for all 50 states + DC."""

from __future__ import annotations

import csv
import json
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PROCESSED = ROOT / "data" / "processed"
API = "https://www.nationsreportcard.gov/DataService/GetAdhocData.aspx"

# Approximate geographic centers for a simple US state map.
CENTROIDS = {
    "AL": (32.806671, -86.791130),
    "AK": (61.370716, -152.404419),
    "AZ": (33.729759, -111.431221),
    "AR": (34.969704, -92.373123),
    "CA": (36.116203, -119.681564),
    "CO": (39.059811, -105.311104),
    "CT": (41.597782, -72.755371),
    "DE": (39.318523, -75.507141),
    "DC": (38.897438, -77.026817),
    "FL": (27.766279, -81.686783),
    "GA": (33.040619, -83.643074),
    "HI": (21.094318, -157.498337),
    "ID": (44.240459, -114.478828),
    "IL": (40.349457, -88.986137),
    "IN": (39.849426, -86.258278),
    "IA": (42.011539, -93.210526),
    "KS": (38.526600, -96.726486),
    "KY": (37.668140, -84.670067),
    "LA": (31.169546, -91.867805),
    "ME": (44.693947, -69.381927),
    "MD": (39.063946, -76.802101),
    "MA": (42.230171, -71.530106),
    "MI": (43.326618, -84.536095),
    "MN": (45.694454, -93.900192),
    "MS": (32.741646, -89.678696),
    "MO": (38.456085, -92.288368),
    "MT": (46.921925, -110.454353),
    "NE": (41.125370, -98.268082),
    "NV": (38.313515, -117.055374),
    "NH": (43.452492, -71.563896),
    "NJ": (40.298904, -74.521011),
    "NM": (34.840515, -106.248482),
    "NY": (42.165726, -74.948051),
    "NC": (35.630066, -79.806419),
    "ND": (47.528912, -99.784012),
    "OH": (40.388783, -82.764915),
    "OK": (35.565342, -96.928917),
    "OR": (43.804133, -120.554201),
    "PA": (40.590752, -77.209755),
    "RI": (41.680893, -71.511780),
    "SC": (33.856892, -80.945007),
    "SD": (44.299782, -99.438828),
    "TN": (35.747845, -86.692345),
    "TX": (31.054487, -97.563461),
    "UT": (40.150032, -111.862434),
    "VT": (44.045876, -72.710686),
    "VA": (37.769337, -78.169968),
    "WA": (47.400902, -121.490494),
    "WV": (38.491226, -80.954453),
    "WI": (44.268543, -89.616508),
    "WY": (42.755966, -107.302490),
}

STATE_ABBR = list(CENTROIDS.keys())


def percentile_rank(value: float | None, values: list[float | None], *, higher_is_need: bool) -> float | None:
    xs = [v for v in values if v is not None]
    if value is None or len(xs) < 2:
        return None
    if higher_is_need:
        n = sum(1 for v in xs if v <= value)
    else:
        n = sum(1 for v in xs if v >= value)
    return 100.0 * (n - 1) / (len(xs) - 1)


def fetch(params: dict) -> list[dict]:
    query = urllib.parse.urlencode(params, safe=",:")
    url = f"{API}?{query}"
    with urllib.request.urlopen(url, timeout=60) as resp:
        payload = json.loads(resp.read().decode("utf-8"))
    if payload.get("status") != 200:
        raise RuntimeError(f"NAEP API {payload.get('status')}: {url}")
    return payload.get("result") or []


def index_rows(rows: list[dict]) -> dict[tuple, dict]:
    out = {}
    for row in rows:
        if not row.get("isStatDisplayable"):
            continue
        key = (
            str(row.get("jurisdiction")),
            int(row.get("grade")),
            str(row.get("variable")),
            str(row.get("varValueLabel") or row.get("varValue")),
        )
        out[key] = row
    return out


def val(idx: dict, abbr: str, grade: int, variable: str, label: str) -> float | None:
    row = idx.get((abbr, grade, variable, label))
    if row is None:
        return None
    try:
        return float(row["value"])
    except (KeyError, TypeError, ValueError):
        return None


def mean2(a: float | None, b: float | None) -> float | None:
    xs = [x for x in (a, b) if x is not None]
    if not xs:
        return None
    return sum(xs) / len(xs)


def need_score(rec: dict, ela_vals: list, ell_vals: list, below_vals: list) -> float | None:
    parts = [
        (percentile_rank(rec["pct_reading"], ela_vals, higher_is_need=False), 0.60),
        (percentile_rank(rec["ell_gap"], ell_vals, higher_is_need=True), 0.25),
        (
            percentile_rank(
                None if rec["gap_vs_nation"] is None else -rec["gap_vs_nation"],
                below_vals,
                higher_is_need=True,
            ),
            0.15,
        ),
    ]
    weighted = 0.0
    weight = 0.0
    for value, w in parts:
        if value is not None:
            weighted += value * w
            weight += w
    return None if weight == 0 else round(weighted / weight, 1)


def main() -> None:
    jurisdictions = ",".join(STATE_ABBR + ["NP"])
    idx: dict[tuple, dict] = {}
    idx_prior: dict[tuple, dict] = {}
    for year, bucket in ((2024, idx), (2022, idx_prior)):
        for grade in (4, 8):
            for variable in ("TOTAL", "LEP"):
                rows = fetch(
                    {
                        "type": "data",
                        "subject": "reading",
                        "grade": grade,
                        "subscale": "RRPCM",
                        "variable": variable,
                        "jurisdiction": jurisdictions,
                        "stattype": "ALC:AP",
                        "Year": year,
                    }
                )
                bucket.update(index_rows(rows))
                print(f"{year} grade {grade} {variable}: {len(rows)} rows")

    nation_g4 = val(idx, "NP", 4, "TOTAL", "All students")
    nation_g8 = val(idx, "NP", 8, "TOTAL", "All students")
    nation = mean2(nation_g4, nation_g8)

    recs = []
    for abbr, (lat, lng) in CENTROIDS.items():
        g4 = val(idx, abbr, 4, "TOTAL", "All students")
        g8 = val(idx, abbr, 8, "TOTAL", "All students")
        reading = mean2(g4, g8)
        ell4 = val(idx, abbr, 4, "LEP", "ELL")
        not4 = val(idx, abbr, 4, "LEP", "Not ELL")
        ell8 = val(idx, abbr, 8, "LEP", "ELL")
        not8 = val(idx, abbr, 8, "LEP", "Not ELL")
        ell = mean2(ell4, ell8)
        fluent = mean2(not4, not8)
        ell_gap = None if ell is None or fluent is None else round(fluent - ell, 2)
        g4p = val(idx_prior, abbr, 4, "TOTAL", "All students")
        g8p = val(idx_prior, abbr, 8, "TOTAL", "All students")
        reading_prior = mean2(g4p, g8p)
        ell4p = val(idx_prior, abbr, 4, "LEP", "ELL")
        not4p = val(idx_prior, abbr, 4, "LEP", "Not ELL")
        ell8p = val(idx_prior, abbr, 8, "LEP", "ELL")
        not8p = val(idx_prior, abbr, 8, "LEP", "Not ELL")
        ell_prior = mean2(ell4p, ell8p)
        fluent_prior = mean2(not4p, not8p)
        recs.append(
            {
                "state_abbr": abbr,
                "state_name": None,
                "lat": lat,
                "lng": lng,
                "year": 2024,
                "pct_grade4": None if g4 is None else round(g4, 2),
                "pct_grade8": None if g8 is None else round(g8, 2),
                "pct_reading": None if reading is None else round(reading, 2),
                "pct_ell": None if ell is None else round(ell, 2),
                "pct_not_ell": None if fluent is None else round(fluent, 2),
                "ell_gap": ell_gap,
                "nation_pct_reading": None if nation is None else round(nation, 2),
                "gap_vs_nation": None if reading is None or nation is None else round(reading - nation, 2),
                "pct_reading_2022": None if reading_prior is None else round(reading_prior, 2),
                "ell_gap_2022": None
                if ell_prior is None or fluent_prior is None
                else round(fluent_prior - ell_prior, 2),
                "change_vs_2022": None
                if reading is None or reading_prior is None
                else round(reading - reading_prior, 2),
            }
        )

    # Fill official names from a TOTAL row.
    names = {}
    for (abbr, grade, variable, label), row in idx.items():
        if variable == "TOTAL" and abbr not in names:
            names[abbr] = row.get("jurisLabel") or abbr
    for rec in recs:
        rec["state_name"] = names.get(rec["state_abbr"], rec["state_abbr"])

    ela_vals = [r["pct_reading"] for r in recs]
    ell_vals = [r["ell_gap"] for r in recs]
    below_vals = [None if r["gap_vs_nation"] is None else -r["gap_vs_nation"] for r in recs]
    for rec in recs:
        rec["need_score_us"] = need_score(rec, ela_vals, ell_vals, below_vals)

    recs.sort(key=lambda r: (-(r["need_score_us"] or -1), r["state_name"]))
    PROCESSED.mkdir(parents=True, exist_ok=True)
    fields = [
        "state_abbr",
        "state_name",
        "lat",
        "lng",
        "year",
        "pct_grade4",
        "pct_grade8",
        "pct_reading",
        "pct_ell",
        "pct_not_ell",
        "ell_gap",
        "gap_vs_nation",
        "need_score_us",
        "nation_pct_reading",
        "pct_reading_2022",
        "ell_gap_2022",
        "change_vs_2022",
    ]
    out = PROCESSED / "us_states.csv"
    with out.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(recs)
    print(f"Wrote {len(recs)} states to {out}")
    print(f"National public NAEP reading at/above proficient (grades 4 & 8 mean): {nation}")


if __name__ == "__main__":
    main()
