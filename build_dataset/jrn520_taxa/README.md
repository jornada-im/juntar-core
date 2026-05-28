# Jornada taxonomic lists

## jrn520001: Jornada LTER plant list

Data preparation files are in `Documents_Jornada-FC-IM/jornada_im/dataprep/jrn520_taxa/plants`. A guide to the subdirectory contents is below

* The primary Jornada plant list is `jrn_plant_list_YYYYMMDD.csv`, which is exported from the in-progress `jrn_plant_list_MAIN.xlsx` spreadsheet. This list was first published to EDI in 2018 (filename: `JRN vascular plant species list.csv`) and has been updated a few times since publishing.
    - Darren began by compiling from a reformatted version of John Anderson’s list (`Plntalfa_table_edit.xlsx`) and Justin VanZee’s list (`plantlistJER.xlsx`), both now archived in `archive/`.
* There are numerous primary data source tables for creating and cross-checking the plant list above. 
    - A copy of John Anderson's current plant list (`0_test-merge1.xlsx`), sourced from `Documents_Jornada-FC-IM/dataentry/_postprocessing/`, is used as a reference for in-use LTER codes and to populate trait data in the plant list. This integrates his earlier files...
    - A plant list derived from Kelly Allred's Flora of the Jornada Plain (`allred_jornada_spp_table_claude_v3.csv`). This was created from the PDF of the Flora (`allred-jornada-pocket-size-9th.pdf`) using Claude. The scripts and some of the outputs that Claude generated are in `claude_scripts/`.
        - version 4 of this has some hand editing and notes. It is common for Kelly's USDA codes to be incorrect.
    - The USDA Plants (`plantlst_YYYYMMDD.txt`) and Unknown Symbols (`UnknownSymbols_20260318.txt`) lists come directly from the [USDA Plants database](https://plants.sc.egov.usda.gov/downloads)
* `archive/` has files that are obsolete and have been archived, including John and Justin's Excel files mentioned above, and even earlier versions of them. The `archive/JRN_vascular_plant_species_list_DJames.csv` version was a prior working version of the main list that is now superseded.
* `jrn_plant_list_notes.md` contains notes on duplicate codes, unresolveable taxa, and more. Formerly this was `additional_info.txt`.
* `check_2022/` has some old checks on earlier versions of the list.

Note that the field codes list `lter_field_codes_YYYYMMDD.csv`, which has all LTER codes and alternate ones, is in `field_codes` one directory above. The `field_codes/archive/LTER_to_USDA_PLANTS_codes.csv` is the most recent version of the field codes list before reformatting.

To publish the main plant list, export a CSV (`jrn520001_taxa_plants.csv`) to the appropriate output folder.



## jrn520002: Jornada vertebrates list

## jrn520003: Jornada invertebrates list

## jrn520005: Jornada field codes