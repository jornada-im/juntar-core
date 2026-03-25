# README

NOTE! These files come from the `ppt` folder of the NPP-data repository. They were formerly used to generate precipitation data used for analysis of NPP data. They may or may not still be useful. Original documentation from that folder below.

## Precipitation (PPT)

The 'ppt' folder contains data for the rain gauges at the NPP sites. Raw data from the Jornada as well as temporal aggrgegations are contained here. 

The precipitation (ppt) data used for current NPP analyses are from graduated rain gauges (GRGs) at each of the 15 NPP sites. The raw data (Jornada_002001_npp_precipitation_graduated_raingauge_monthly_data.csv) is hosted on EDI but was last downloaded 02/27/2019 (see the *raw* folder). 

The data set version page on EDI:
[doi:10.6073/pasta/81ebec910755e84bb5619832d415ab11](https://doi.org/10.6073/pasta/81ebec910755e84bb5619832d415ab11)

Filtered and summarized data can be found in the *processed* folder and the scripts to create them can be found in the *scripts* folder. Summaries include monthly and annual, where annual has three types:

* annual: the sum of months in the same calendar year
* water year: The sum of months in the same water year (Oct-Sept)
* growing season: The sum of Apr-Aug, the growing season (seasonal value, but assigned to each year, so considered 'annual' summary)

Note: The different sites had ppt records start at different times in 1989-1990. If you are interested in these first few years, take that into consideration that the annual summaries will be underestimated. Same goes for the latest water year.