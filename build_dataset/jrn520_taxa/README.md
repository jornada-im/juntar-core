# Jornada taxonomic lists

## jrn520001: Jornada plant list

Data preparation files are in `Documents_Jornada-FC-IM/jornada_im/dataprep/jrn520_taxa/plants`. A guide to the subdirectory contents is below

* The primary Jornada plant list is `jornada_plant_list_YYYYMMDD.csv`, which is exported from the in-progress `jornada_plant_list_MAIN.xlsx` spreadsheet. This list was first published to EDI in 2018 (filename: `JRN vascular plant species list.csv`) and has been updated a few times since publishing.
* `source_data` contains the primary data sources for creating the plant list above. Darren began by compiling from a reformatted version of John Anderson’s list (`Plntalfa_table_edit.xlsx`) and Justin VanZee’s list (`plantlistJER.xlsx`), both now archived in `source_data/archive/`.
    - A copy of John Anderson's current plant list, sourced from `Documents_Jornada-FC-IM/dataentry/_postprocessing/0_test-merge1.xlsx`, is used to populate trait data in the plant list. This integrates his earlier files...
    - The USDA Plants (`plantlst_YYYYMMDD.txt`) and Unknown Symbols (`UnknownSymbols_20260318.txt`) lists come directly from the [USDA Plants database](https://plants.sc.egov.usda.gov/downloads)
    - `source_data/archive` has files that are obsolete and have been archived, including John and Justin's Excel files mentioned above, an even earlier versions of them. The `archive/JRN_vascular_plant_species_list_DJames.csv` version was a prior working version of the main list that is now superseded.
* Allred list
* Codes list `archive/LTER_to_USDA_PLANTS_codes.csv` is the most recent version of the field codes list before reformatting.
* Check 2022
* Additional info


To publish the main plant list, export a CSV (`jrn520001_taxa_plants.csv`) to the appropriate output folder.

## jrn520002: Jornada vertebrates list

## jrn520003: Jornada invertebrates list

## jrn520005: Jornada field codes