# Climate Monitoring in Southeast Utah National Parks 
This project downloads and summarizes Co-op weather station data from Arches (ARCH) and Canyonlands (CANY) national parks, Hovenweep (HOVE) and Natural Bridges (NABR) national monuments, and the City of Moab, UT  from the [Global Historical Climatology Network - Daily (GHCND)](https://www.ncdc.noaa.gov/ghcn-daily-description) using R and the [rnoaa](https://cran.r-project.org/web/packages/rnoaa/) package (Chamberlain et al., 2020).  

## Description
This repository contains a series of R and RMarkdown scripts that download and summarize data from the GHCND.

* First [downloadData.R](downloadData.R) is used to download Co-op weather station data from the GCHND for ARCH, CANY, HOVE, NABR, and Moab, then saves the data in the climateData.RData file.

* After the data are downloaded, [wySummary.Rmd](wySummary.Rmd), or [wySummary_CANY.Rmd](wySummary_CANY.Rmd) can be used to produce temperature and precipitaion summaries for a given park unit and water year.
  - [wySummary.Rmd](wySummary.Rmd) produces a brief automated report summarizing temperature and  precipitation for ARCH, HOVE, NABR, and Moab. See [20201105_ARCH_ClimateSumary_WY2020.pdf](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/20201105_ARCH_ClimateSumary_WY2020.pdf).
  - [wySummary_CANY.Rmd](wySummary_CANY.Rmd) produces a similar report specific to CANY and it's districts (Island in the Sky, The Maze, and The Needles).
  - [wySummary_figures.R](wySummary_figures.R) produces mulit-panel figures that include all the parks/districts. This scripts need to be revised, but examples can be seen in the [Results sub-directory](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results).
  
* The [renderSummaries.R](renderSummaries.R) script runs parameterized reports (e.g., my.park = "ARCH" and water.yr = 2020) that call wySummary.Rmd or wySummary_CANY.Rmd to render reports. The PDF's are then named with a date stamp and 4-letter park code and saved in the [Results sub-directory](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results).

## Figures
Example of figure produced from wySummary.R. This figure is of temperature trends for water year 2020.
![20201105_2020_Temp Trends.png](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/20201105_2020_Temp%20Trends.png)

## Authors
* **Matthew Van Scoyoc** - *Initial work* - [scoyoc](https://github.com/scoyoc)

# License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
