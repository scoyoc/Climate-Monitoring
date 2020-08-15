# Southeast Utah National Park Unit Climate Monitoring
This project downloads and summarizes Co-op weather data from Arches and 
Canyonlands National Park, Hovenweep and Natural Bridges National Monuments, and
Moab, UT  from the [Global Historic Climatology Network Daily (GHCND)](https://www.ncdc.noaa.gov/ghcn-daily-description) data using R and the [rnoaa](https://cran.r-project.org/web/packages/rnoaa/) package (Chamberlain et. al, 
2019).  

## Description
This repository contains a series of *.R and *.Rmd scripts that download and summarize GHCND.

* First use [downloadData.R](downloadData.R) to download GCHND data from the Arches 
and Canyonlands National Parks and Hovenweep and Natural Bridges National Monuments
Co-op stations at the visitor centers. This script saves the data in the Climate.RData
file.
* Once the data are downloaded, [monthlySummary.R](monthlySummary.R), 
[ClimateSummary_CANY.Rmd](ClimateSummary_CANY.Rmd), or [ClimateSummary_PARK.Rmd](ClimateSummary_PARK.Rmd) can be used to produce summaries of the data for a given water year.
  - [monthlySummary.R](monthlySummary.R) produces four figures summarizing temperature and  precipitation for each park and Moab, Utah.
  - [ClimateSummary_CANY.Rmd](ClimateSummary_CANY.Rmd) filters the data by a specific Canyonlands National Park and then produced a report that summarizes temperature and precipitation data for each district of the Park.
  - [ClimateSummary_PARK.Rmd](ClimateSummary_PARK.Rmd) filters the data by the park unit or Moab as specified by the user. To change the park unit simpley change the title and the definition of the **my.dist** object in the first chunk (e.g., **my.dist <- "ARCH"** to filter data for Arches National Park). This script then produced a report that summarizes temperature and precipitation data for that park unit or Moab. View [20200815_ClimateSummary_ARCH.pdf](20200815_ClimateSummary_ARCH.pdf) documents for an example of this report.

### Examples of figures produced using [monthlySummary.R](monthlySummary.R)

![2020-08-14_2020_Temp Trends.png](2020-08-14_2020_Temp Trends.png)

![2020-08-14_2020_PRCP Trends.png](2020-08-14_2020_PRCP Trends.png)

## Authors
* **Matthew Van Scoyoc** - *Initial work* - [scoyoc](https://github.com/scoyoc)

# License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
