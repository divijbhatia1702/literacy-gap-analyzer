"""Build California school-level ELA tables from CAASPP research files."""

from __future__ import annotations

import csv
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "data" / "raw"
PROCESSED = ROOT / "data" / "processed"

TEST_ELA = "1"
GRADE_ALL = "13"
TYPE_STATE = "4"
TYPE_COUNTY = "5"
TYPE_DISTRICT = "6"
TYPE_SCHOOLS = {"7", "9", "10"}

GROUP_ALL = "1"
GROUP_SED = "31"
GROUP_NOT_SED = "111"
GROUP_ELL = "160"
GROUP_FLUENT = "6"

KEEP_GROUPS = {GROUP_ALL, GROUP_SED, GROUP_NOT_SED, GROUP_ELL, GROUP_FLUENT}
KEEP_TYPES = {TYPE_STATE, TYPE_COUNTY, TYPE_DISTRICT} | TYPE_SCHOOLS

ELA_CANDIDATES = [
    RAW / "sb_ca2025_all_csv_ela_v1.txt",
    RAW / "sb_ca2025_all_07_csv_v1.txt",
]


def to_float(value: str | None) -> float | None:
    text = (value or "").strip()
    if text in {"", "*", "--", "NA", "N/A"}:
        return None
    try:
        return float(text.replace(",", ""))
    except ValueError:
        return None


def pad_zip(value: str | None) -> str:
    digits = "".join(ch for ch in (value or "") if ch.isdigit())
    if not digits:
        return ""
    return digits.zfill(5)[-5:]


def school_id(county: str, district: str, school: str) -> str:
    return f"{county}-{district}-{school}"


def skip_school(name: str, school_code: str) -> bool:
    if school_code in {"0000000"}:
        return True
    lowered = (name or "").lower()
    return "district level program" in lowered or "nonpublic" in lowered


def percentile_rank(value: float | None, values: list[float | None], *, higher_is_need: bool) -> float | None:
    xs = [v for v in values if v is not None]
    if value is None or len(xs) < 2:
        return None
    if higher_is_need:
        n = sum(1 for v in xs if v <= value)
    else:
        n = sum(1 for v in xs if v >= value)
    return 100.0 * (n - 1) / (len(xs) - 1)


def need_score(rec: dict, ela_vals: list, econ_vals: list, ell_vals: list, below_vals: list, below_key: str) -> float | None:
    below = None if rec[below_key] is None else -rec[below_key]
    parts = [
        (percentile_rank(rec["pct_ela_all"], ela_vals, higher_is_need=False), 0.50),
        (percentile_rank(rec["economic_gap"], econ_vals, higher_is_need=True), 0.25),
        (percentile_rank(rec["ell_gap"], ell_vals, higher_is_need=True), 0.15),
        (percentile_rank(below, below_vals, higher_is_need=True), 0.10),
    ]
    weighted = 0.0
    weight = 0.0
    for value, w in parts:
        if value is not None:
            weighted += value * w
            weight += w
    return None if weight == 0 else round(weighted / weight, 1)


def load_entities() -> tuple[dict[str, dict], dict[str, str], dict[str, str]]:
    path = RAW / "sb_ca2025entities_csv.txt"
    schools: dict[str, dict] = {}
    counties: dict[str, str] = {}
    districts: dict[str, str] = {}
    with path.open(encoding="utf-8", errors="replace", newline="") as handle:
        for row in csv.DictReader(handle, delimiter="^"):
            county = row["County Code"].strip()
            district = row["District Code"].strip()
            school = row["School Code"].strip()
            type_id = str(row["Type ID"]).strip()
            if type_id == TYPE_COUNTY:
                counties[county] = (row.get("County Name") or "").strip()
            if type_id == TYPE_DISTRICT:
                districts[school_id(county, district, "0000000")] = (row.get("District Name") or "").strip()
            if type_id in TYPE_SCHOOLS:
                sid = school_id(county, district, school)
                schools[sid] = {
                    "county_code": county,
                    "district_code": district,
                    "school_code": school,
                    "county_name": (row.get("County Name") or "").strip(),
                    "district_name": (row.get("District Name") or "").strip(),
                    "school_name": (row.get("School Name") or "").strip(),
                    "zip": pad_zip(row.get("Zip Code")),
                    "type_id": type_id,
                }
    return schools, counties, districts


def load_zip_centroids() -> dict[str, tuple[float, float]]:
    path = RAW / "2024_Gaz_zcta_national.txt"
    centroids: dict[str, tuple[float, float]] = {}
    if not path.exists():
        return centroids
    with path.open(encoding="utf-8", errors="replace") as handle:
        header = handle.readline()
        delim = "\t" if "\t" in header else None
        for line in handle:
            parts = line.split("\t") if delim else line.split()
            if len(parts) < 7:
                continue
            zcta = pad_zip(parts[0])
            try:
                lat = float(parts[5].strip())
                lon = float(parts[6].strip())
            except ValueError:
                continue
            centroids[zcta] = (lat, lon)
    return centroids


def ela_path() -> Path:
    for path in ELA_CANDIDATES:
        if path.exists():
            return path
    raise FileNotFoundError("Missing CAASPP ELA research file. Run scripts/download_caaspp.sh")


def stream_aggregates(path: Path) -> tuple[dict, dict, dict, dict]:
    schools: dict[str, dict] = {}
    district_pct: dict[str, float] = {}
    county_pct: dict[str, float] = {}
    state_pct = None

    with path.open(encoding="utf-8", errors="replace", newline="") as handle:
        reader = csv.DictReader(handle, delimiter="^")
        for row in reader:
            type_id = str(row.get("Type ID", "")).strip()
            group = str(row.get("Student Group ID", "")).strip()
            grade = str(row.get("Grade", "")).strip()
            test_id = str(row.get("Test ID", "")).strip()
            if test_id not in {"", TEST_ELA} and test_id != TEST_ELA:
                continue
            if grade != GRADE_ALL or group not in KEEP_GROUPS or type_id not in KEEP_TYPES:
                continue
            # Statewide ELA file is ELA-only; county combined file still has Test ID.
            if "Test ID" in row and row["Test ID"].strip() not in {"", TEST_ELA}:
                continue

            county = row["County Code"].strip()
            district = row["District Code"].strip()
            school = row["School Code"].strip()
            pct = to_float(row.get("Percentage Standard Met and Above"))
            n_tested = to_float(row.get("Total Students Tested with Scores"))
            n_enrolled = to_float(row.get("Total Students Enrolled"))

            if type_id == TYPE_STATE and group == GROUP_ALL:
                state_pct = pct
                continue
            if type_id == TYPE_COUNTY and group == GROUP_ALL:
                county_pct[county] = pct
                continue
            if type_id == TYPE_DISTRICT and group == GROUP_ALL:
                district_pct[school_id(county, district, "0000000")] = pct
                continue
            if type_id not in TYPE_SCHOOLS:
                continue
            if skip_school(row.get("School Name", ""), school):
                continue

            sid = school_id(county, district, school)
            rec = schools.get(sid)
            if rec is None:
                rec = {
                    "county_code": county,
                    "district_code": district,
                    "school_code": school,
                    "district_name": (row.get("District Name") or "").strip(),
                    "school_name": (row.get("School Name") or "").strip(),
                    "test_year": (row.get("Test Year") or "").strip(),
                    "n_enrolled": None,
                    "n_tested": None,
                    "pct_ela_all": None,
                    "pct_ela_sed": None,
                    "pct_ela_not_sed": None,
                    "pct_ela_ell": None,
                    "pct_ela_fluent": None,
                    "n_sed_tested": None,
                    "n_ell_tested": None,
                }
                schools[sid] = rec

            if group == GROUP_ALL:
                rec["pct_ela_all"] = pct
                rec["n_tested"] = n_tested
                rec["n_enrolled"] = n_enrolled
                rec["school_name"] = rec["school_name"] or (row.get("School Name") or "").strip()
                rec["district_name"] = rec["district_name"] or (row.get("District Name") or "").strip()
            elif group == GROUP_SED:
                rec["pct_ela_sed"] = pct
                rec["n_sed_tested"] = n_tested
            elif group == GROUP_NOT_SED:
                rec["pct_ela_not_sed"] = pct
            elif group == GROUP_ELL:
                rec["pct_ela_ell"] = pct
                rec["n_ell_tested"] = n_tested
            elif group == GROUP_FLUENT:
                rec["pct_ela_fluent"] = pct

    return schools, district_pct, county_pct, state_pct


def write_csv(path: Path, rows: list[dict], fields: list[str]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, extrasaction="ignore")
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def main() -> None:
    PROCESSED.mkdir(parents=True, exist_ok=True)
    entities, county_names, district_names = load_entities()
    centroids = load_zip_centroids()
    scores, district_pct, county_pct, state_pct = stream_aggregates(ela_path())

    recs = []
    for sid, rec in scores.items():
        if rec.get("pct_ela_all") is None:
            continue
        meta = entities.get(sid, {})
        county = rec["county_code"]
        dkey = school_id(county, rec["district_code"], "0000000")
        rec["county_name"] = meta.get("county_name") or county_names.get(county, "")
        rec["district_name"] = rec["district_name"] or meta.get("district_name") or district_names.get(dkey, "")
        rec["school_name"] = rec["school_name"] or meta.get("school_name", "")
        rec["zip"] = meta.get("zip", "")
        rec["cds"] = sid.replace("-", "")
        rec["state_pct_ela"] = state_pct
        rec["county_pct_ela"] = county_pct.get(county)
        rec["district_pct_ela"] = district_pct.get(dkey)
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
        rec["gap_vs_state"] = None if rec["pct_ela_all"] is None or state_pct is None else round(rec["pct_ela_all"] - state_pct, 2)
        rec["gap_vs_district"] = (
            None
            if rec["pct_ela_all"] is None or rec["district_pct_ela"] is None
            else round(rec["pct_ela_all"] - rec["district_pct_ela"], 2)
        )
        recs.append(rec)

    ela_vals = [r["pct_ela_all"] for r in recs]
    econ_vals = [r["economic_gap"] for r in recs]
    ell_vals = [r["ell_gap"] for r in recs]
    below_state = [None if r["gap_vs_state"] is None else -r["gap_vs_state"] for r in recs]
    for rec in recs:
        rec["need_score_ca"] = need_score(rec, ela_vals, econ_vals, ell_vals, below_state, "gap_vs_state")

    by_district: dict[str, list] = defaultdict(list)
    for rec in recs:
        by_district[school_id(rec["county_code"], rec["district_code"], "0000000")].append(rec)
    for group in by_district.values():
        ela = [r["pct_ela_all"] for r in group]
        econ = [r["economic_gap"] for r in group]
        ell = [r["ell_gap"] for r in group]
        below = [None if r["gap_vs_district"] is None else -r["gap_vs_district"] for r in group]
        for rec in group:
            rec["need_score_district"] = need_score(rec, ela, econ, ell, below, "gap_vs_district")

    recs.sort(key=lambda r: (-(r["need_score_ca"] or -1), r["county_name"], r["district_name"], r["school_name"]))

    school_fields = [
        "county_code",
        "county_name",
        "district_code",
        "district_name",
        "school_code",
        "school_name",
        "cds",
        "zip",
        "test_year",
        "n_enrolled",
        "n_tested",
        "pct_ela_all",
        "pct_ela_sed",
        "pct_ela_not_sed",
        "pct_ela_ell",
        "pct_ela_fluent",
        "n_sed_tested",
        "n_ell_tested",
        "economic_gap",
        "ell_gap",
        "gap_vs_state",
        "gap_vs_district",
        "need_score_ca",
        "need_score_district",
        "state_pct_ela",
        "county_pct_ela",
        "district_pct_ela",
    ]
    write_csv(PROCESSED / "ca_schools.csv", recs, school_fields)

    srvusd = [
        r
        for r in recs
        if r["county_code"] == "07" and r["district_code"] == "61804"
    ]
    srvusd.sort(key=lambda r: (-(r["need_score_district"] or -1), r["school_name"]))
    write_csv(PROCESSED / "srvusd_schools.csv", srvusd, school_fields)

    zip_rows = []
    by_zip: dict[str, list] = defaultdict(list)
    for rec in recs:
        if rec["zip"]:
            by_zip[rec["zip"]].append(rec)
    for zip_code, group in sorted(by_zip.items()):
        elas = [r["pct_ela_all"] for r in group if r["pct_ela_all"] is not None]
        needs = [r["need_score_ca"] for r in group if r["need_score_ca"] is not None]
        latlon = centroids.get(zip_code)
        if not latlon or not elas:
            continue
        zip_rows.append(
            {
                "zip": zip_code,
                "lat": round(latlon[0], 6),
                "lng": round(latlon[1], 6),
                "n_schools": len(group),
                "n_tested": round(sum(r["n_tested"] or 0 for r in group), 0),
                "pct_ela_all": round(sum(elas) / len(elas), 2),
                "need_score_ca": None if not needs else round(sum(needs) / len(needs), 1),
                "counties": "; ".join(sorted({r["county_name"] for r in group if r["county_name"]})),
            }
        )
    write_csv(
        PROCESSED / "ca_zips.csv",
        zip_rows,
        ["zip", "lat", "lng", "n_schools", "n_tested", "pct_ela_all", "need_score_ca", "counties"],
    )

    print(f"California schools: {len(recs)}")
    print(f"SRVUSD schools: {len(srvusd)}")
    print(f"ZIP points: {len(zip_rows)}")
    print(f"California ELA met-and-above: {state_pct}")


if __name__ == "__main__":
    main()
