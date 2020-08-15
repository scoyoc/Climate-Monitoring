# Southeast Utah National Park Unit Climate Monitoring
This project downloads and summarizes Co-op weather data from Arches and 
Canyonlands National Park, Hovenweep and Natural Bridges National Monuments, and
Moab, UT  from the [Global Historic Climatology Network Daily (GHCND)](https://www.ncdc.noaa.gov/ghcn-daily-description) data using R and the [rnoaa](https://cran.r-project.org/web/packages/rnoaa/) package (Chamberlain et. al, 
2019).  

## Description
This repository contains a series of .R and .Rmd scripts that download and summarize data from the GHCND.

* First use [downloadData.R](downloadData.R) to download GCHND data from the Arches 
and Canyonlands national parks, Hovenweep and Natural Bridges national monuments, and the Moab, Utah Co-op stations. This script saves the data in the Climate.RData file.
* Once the data are downloaded, [MonthlySummary.R](MonthlySummary.R), [ClimateSummary_CANY.Rmd](ClimateSummary_CANY.Rmd), or [ClimateSummary_PARK.Rmd](ClimateSummary_PARK.Rmd) can be used to produce summaries of the data for a given water year.
  - [MonthlySummary.R](MonthlySummary.R) produces four figures summarizing temperature and  precipitation for each park and Moab, Utah.
  - [ClimateSummary_CANY.Rmd](ClimateSummary_CANY.Rmd) filters the data for Canyonlands National Park and then produces a report that summarizes temperature and precipitation data for each district of the Park. See [20200815_ClimateSummary_CANY.pdf](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/20200815_ClimateSummary_CANY.pdf).
  - [ClimateSummary_PARK.Rmd](ClimateSummary_PARK.Rmd) filters the data by a given park unit or Moab as specified by the user. To change the park unit simpley change the title and the definition of the **my.dist** object in the first chunk (e.g., **my.dist <- "ARCH"** to filter data for Arches National Park; a list of dictric ID's can be found in *stations$district*). The script then produces a report that summarizes temperature and precipitation data for that park unit or Moab. See [20200815_ClimateSummary_ARCH.pdf](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/20200815_ClimateSummary_ARCH.pdf).

### Examples of figures produced using [MonthlySummary.R](MonthlySummary.R)
**Figure 1.**
![2020-08-14_Temperature Departures.png](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/2020-08-14_Temperature%20Departures.png)

**Figure 2.**
![2020-08-14_2020_Temp Trends.png](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/2020-08-14_2020_Temp%20Trends.png)

**Figure 3.**
![2020-08-14_Precipitation.png](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/2020-08-14_Precipitation.png)

**Figure 4.**
![2020-08-14_2020_PRCP Trends.png](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/2020-08-14_2020_PRCP%20Trends.png)

## Authors
* **Matthew Van Scoyoc** - *Initial work* - [scoyoc](https://github.com/scoyoc)

# License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
