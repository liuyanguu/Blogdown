# Africa population extract

Source: United Nations, Department of Economic and Social Affairs, Population Division (2024). *World Population Prospects 2024, Online Edition*.

[Official workbook: population by single age, both sexes](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20%28Standard%29/EXCEL_FILES/2_Population/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx)

Retrieved 12 November 2026. License: CC BY 3.0 IGO.

`WPP2024_Africa_PopulationByAge1GroupSex_Medium.csv` contains the Africa aggregate (location code 903), including Northern Africa. Population is on 1 July, both sexes combined, in **thousands**. The extract includes single ages 0–24 for 2015, 2020, 2025, 2030, 2050 and 2100. Historical years use the `Estimates` sheet; 2025 onward uses `Medium variant`.

To reproduce, download the workbook, install `openpyxl`, and run:

```bash
python scripts/extract-africa-population.py /path/to/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx
```

For each table cell, sum single ages below the age cutoff, divide by 1,000 to convert thousands to millions, then round to the nearest million. The original unrounded workbook values are retained in the extract.
