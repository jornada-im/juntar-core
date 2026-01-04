# Updating daily climate for 15 NPP sites through 2019
*Heather Savoy*

*Last Updated: Dec 27, 2019*

Daily climate (precipitation, minimum temperature, and maximum temperature) for the 15 NPP locations were previously estimated by Jin Yao for Jan 1980 – Mar 2014. The 1980-2010 precipitation data are [published](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-jrn.210425001.74), but the temperature and 2011-2014 data can be found in her hard drive backup (see `work/JERclimate/daily_temp` and `work/JERclimate/JERrainfall/create_daily`). 

I’ve worked to:
1.	Update these daily climate data through 2019 with measured observations from wireless sensors 
2.	Replace previous temperature estimates with location-specific estimates 

Precipitation and temperature measurement locations and techniques have evolved across 1980-2019 which is why the assignment of daily climate to these 15 locations changes depending on the dates being filled.

##### Temperature
Jin originally borrowed daily min and max values from the LTER weather station and from the JER HQ. The C-CALI, C-GRAV, C-SAND, and G-SUMM locations borrowed from LTER and the remaining sites borrowed from HQ. When either reference dataset had missing values, a linear regression between the two were used to fill those gaps (e.g., LTER data began in 1983, so a linear model LTER = f(HQ) using 1983-2010 data was fit and used to predict 1980-1982 LTER data from HQ).  

Jin returned to update this work with 2011-2014 temperature data using the same process, but using the WBAN dataset in lieu of the true HQ dataset due to John Anderson's advice (see notes at `work/JERclimate/daily_temp/analysis_2014jun/Notes_daily temp_2011_2014mar_jun18`). 

Matt Petrie took over the Modoki project and started a process to estimate site-specific daily temperature instead of just borrowing values. By this time (2017), the 15 NPP locations had meteorological sensors reporting their own daily data. Now that a dataset of daily data was available per site, regressions can be built between them and the the two reference datasets. He used the same two reference datasets, but built regessions by season (growing season and the remainder of the year) but he was primarily interested in mean temperature values.  

This repo strives to adapt Jin's and Matt's approaches to estimate site-specific min and max temperatures from 1980-present. One issue is the gaps in the new sensor data. Although they were mostly installed in mid-2013, the wireless reporing wasn't operational until last 2015 ish, so there are many gaps leading up to a mostly stable 2016-2019 dataset. Should these gaps be filled in the same way pre-mid-2013 values are estimated? Or should it be modified to use more wireless sensor data? 

![missing temperature data](figures/missing_temp.png)

Also, the two reference datasets may need to be updated through 2019 depending on the chosen gap filling procedure. 

##### Precipitation
For 1980-1988 precipitation, Jin borrowed daily data the closest daily rain gauge for each locaiton. 
Once graduated rain gauges (GRG) were installed at each location in 1989, quasi-monthly values were available for each location. To distribute these GRG precipitation totals among the days in between measurements, nearby daily precipitation measurements were used to determine the proportion of rainfall per day during the collection period. Those proportions were then mutliplied by the GRG measurement to estimate site-specific daily precipitation. For a more detailed explanation and the site-specific borrowing rules, see the content in the compressed folder `work/JERclimate/JERrainfall/create_daily/estimate_2014jun/To_John_2014jun11.zip`. This procedure was applied through mid-2013.

In mid-2013, the new meteorlogical sensors were installed at the 15 locations, by the wireless reporting wasn't operational until late 2015, similar to temperature. Similar questions arise: should the mid-2013 - late 2015 gaps be filled with the same old grg-proporation procedure? 

![missing precipitation data](figures/missing_ppt.png)

Here's a comparison between preciptation totals in between GRG collection from both the GRGs and the wireless sensors. Only 2016+ data are shown to remove the years of known gaps in wireless sensor data.

![precipitation data comparison](figures/ppt_comparison.png)

