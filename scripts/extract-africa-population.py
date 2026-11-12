"""Extract the post's Africa data from the official WPP 2024 workbook.

Usage: python scripts/extract-africa-population.py /path/to/workbook.xlsx
Requires openpyxl. See Data/WPP2024_Africa_README.md for the download URL.
"""
import csv
import sys
from pathlib import Path

import openpyxl

workbook = openpyxl.load_workbook(sys.argv[1], read_only=True, data_only=True)
records = []
for sheet, years in [("Estimates", {2015, 2020}),
                     ("Medium variant", {2025, 2030, 2050, 2100})]:
    # The publisher stores a truncated worksheet dimension; stream all cells.
    workbook[sheet].reset_dimensions()
    found = set()
    for row in workbook[sheet].iter_rows(min_row=18, values_only=True):
        if row[4] == 903 and row[10] in years:
            assert row[2] == "Africa", row[:11]
            year = row[10]
            for age in range(25):
                records.append(["Africa", year, age, age, row[11 + age]])
            found.add(year)
            if found == years:
                break
    assert found == years, (sheet, found)
workbook.close()
assert len(records) == 150
output = Path(__file__).resolve().parents[1] / "Data/WPP2024_Africa_PopulationByAge1GroupSex_Medium.csv"
with output.open("w", newline="") as stream:
    writer = csv.writer(stream, lineterminator="\n")
    writer.writerow(["Location", "Time", "AgeGrp", "AgeGrpStart", "PopTotal"])
    writer.writerows(records)
print(output)
